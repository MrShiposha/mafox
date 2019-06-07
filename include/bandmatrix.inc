#ifndef MAFOX_BANDMATRIX_INC
#define MAFOX_BANDMATRIX_INC

#include "bandmatrix.h"
#include "util.h"

namespace mafox
{
    template <typename T>
    class band_matrix_data_t
    {
    public:
        band_matrix_data_t(std::size_t size, std::size_t l, std::size_t u)
        : size(size), l(l), u(u), arrays(new std::unique_ptr<T[]>[1+l+u])
        {
            long long low = static_cast<long long>(l);
            long long up = static_cast<long long>(u);

            long long col = 0;
            long long i = -low;
            for(std::size_t diag_sz = size+i; i <= up; ++i, ++col, diag_sz = size-std::abs(i))
            {
                arrays[col].reset(new T[diag_sz]);
                zero_array(arrays[col].get(), diag_sz * sizeof(T));
            }
        }

        band_matrix_data_t(band_matrix_data_t &other)
        : size(other.size), l(other.l), u(other.u), arrays(new std::unique_ptr<T[]>[1+l+u])
        {
            long long low = static_cast<long long>(l);
            long long up = static_cast<long long>(u);

            long long col = 0;
            long long i = -low;
            for(std::size_t diag_sz = size+i; i <= up; ++i, ++col, diag_sz = size-std::abs(i))
            {
                arrays[col].reset(new T[diag_sz]);
                memcpy(arrays[col].get(), other.arrays[col].get(), diag_sz * sizeof(T));
            }
        }

        band_matrix_data_t(band_matrix_data_t &&other)
        : size(other.size), l(other.l), u(other.u), arrays(std::move(other.arrays)) 
        {}

        static auto make(std::size_t size, std::size_t l, std::size_t u)
        {
            return std::make_shared<band_matrix_data_t<T>>(size, l, u);
        }

        static auto make(band_matrix_data_t &other)
        {
            return std::make_shared<band_matrix_data_t<T>>(other);
        }

        static auto make(band_matrix_data_t &&other)
        {
            return std::make_shared<band_matrix_data_t<T>>(std::forward<band_matrix_data_t>(other));
        }

        bool is_zero(std::size_t i, std::size_t j) const
        {
            return static_cast<long long>(j) < static_cast<long long>(i) - static_cast<long long>(l) 
                || j > i + u;
        }

        const T &c_at(std::size_t i, std::size_t j) const
        {
            if(is_zero(i, j))
                return ZERO<T>;
            else
            {
                std::size_t col = j - (static_cast<long long>(i) - static_cast<long long>(l));
                std::size_t row = (j < i? i - (i-j) : i);

                return arrays[col][row];
            }
        }

        T &at(std::size_t i, std::size_t j)
        {
            assert(!is_zero(i, j));

            std::size_t col = j - (static_cast<long long>(i) - static_cast<long long>(l));
            std::size_t row = (j < i? i - (i-j) : i);

            return arrays[col][row];
        }

        std::size_t size;
        std::size_t l, u;
        std::unique_ptr<std::unique_ptr<T[]>[]> arrays;
    };

    template <typename T>
    BandMatrix<T>::BandMatrix(std::size_t size, std::size_t lower_bandwidth, std::size_t upper_bandwidth)
    : m_data(band_matrix_data_t<T>::make(size, lower_bandwidth, upper_bandwidth))
    {}

    template <typename T>
    BandMatrix<T>::BandMatrix(const BandMatrix &other)
    : m_data(band_matrix_data_t<T>::make(*other.m_data))
    {}

    template <typename T>
    BandMatrix<T>::BandMatrix(BandMatrix &&other)
    : m_data(std::move(other.m_data))
    {}

    template <typename T>
    BandMatrix<T>::BandMatrix(shared_data_t m_data)
    : m_data(m_data)
    {}

    template <typename T>
    BandMatrix<T>::~BandMatrix()
    {}

    template <typename T>
    BandMatrix<T> &BandMatrix<T>::operator=(const BandMatrix &rhs)
    {
        if(this != &rhs)
            *this = std::move(BandMatrix<T>(rhs));

        return *this;
    }

    template <typename T>
    BandMatrix<T> &BandMatrix<T>::operator=(BandMatrix &&rhs)
    {
        if(this != &rhs)
        {
            m_data->size   = rhs.m_data->size;
            m_data->l      = rhs.m_data->l;
            m_data->u      = rhs.m_data->u;
            m_data->arrays = std::move(rhs.m_data->arrays);
        }

        return *this;
    }

    template <typename T>
    std::size_t BandMatrix<T>::rows() const
    {
        return m_data->size;
    }

    template <typename T>
    std::size_t BandMatrix<T>::cols() const
    {
        return m_data->size;
    }

    template <typename T>
    typename BandMatrix<T>::reference BandMatrix<T>::element(std::size_t i, std::size_t j)
    {
        assert(i < rows() && j < cols());

        if(m_data->is_zero(i, j))
            MAFOX_FATAL("Accessing non-const zero element of banded matrix");
        else
            return m_data->at(i, j);
    }

    template <typename T>
    typename BandMatrix<T>::const_reference BandMatrix<T>::element(std::size_t i, std::size_t j) const
    {
        assert(i < rows() && j < cols());

        return m_data->c_at(i, j);
    }

    template <typename T>
    void BandMatrix<T>::set_element(std::size_t i, std::size_t j, const_reference value)
    {
        assert(try_set_element(i, j, value));
    }
    
    template <typename T>
    bool BandMatrix<T>::try_set_element(std::size_t i, std::size_t j, const_reference value)
    {
        assert(i < rows() && j < cols());

        if(m_data->is_zero(i, j))
            return false;
        else
        {
            m_data->at(i, j) = value;
            return true;
        }
    }

    template <typename T>
    void BandMatrix<T>::transpose()
    {
        if(lower_bandwidth() == upper_bandwidth())
        {
            for(std::size_t i = 1, diag_index = lower_bandwidth(); i <= diag_index; ++i)
                m_data->arrays[diag_index - i].swap(m_data->arrays[diag_index + i]);
            std::swap(m_data->l, m_data->u);
        }
        else
        {
            BandMatrix<T> result(m_data->size, m_data->u, m_data->l);
            
            for(std::size_t i = 0, sz = m_data->u+m_data->l; i <= sz; ++i)
                result.m_data->arrays[i] = std::move(m_data->arrays[sz - i]);

            *this = std::move(result);
        }
    }

    template <typename T>
    BandMatrix<T> BandMatrix<T>::transposed()
    {
        BandMatrix<T> result(*this);
        result.transpose();

        return result;
    }

    template <typename T>
    void BandMatrix<T>::transpose_rsd()
    {
        long long i = -static_cast<long long>(m_data->l);
        long long u = static_cast<long long>(m_data->u);
        std::size_t col = 0;

        for(; i <= u; ++i, ++col)
            std::reverse(m_data->arrays[col].get(), m_data->arrays[col].get() + m_data->size-std::abs(i));
    }

    template <typename T>
    BandMatrix<T> BandMatrix<T>::transposed_rsd()
    {
        BandMatrix<T> result(*this);
        result.transpose_rsd();

        return result;
    }

    template <typename T>
    typename BandMatrix<T>::shared_data_t BandMatrix<T>::shared_data()
    {
        return m_data;
    }

    template <typename T>
    typename BandMatrix<T>::const_shared_data_t BandMatrix<T>::shared_cdata() const
    {
        return m_data;
    }

    template <typename T>
    BandMatrix<T> BandMatrix<T>::share()
    {
        return BandMatrix<T>(m_data);
    }

    template <typename T>
    std::shared_ptr<IMatrix<T>> BandMatrix<T>::share_interface()
    {
        return std::shared_ptr<IMatrix<T>>(new BandMatrix<T>(m_data));
    }

    template <typename T>
    std::shared_ptr<const IMatrix<T>> BandMatrix<T>::share_interface() const
    {
        return std::shared_ptr<const IMatrix<T>>(new BandMatrix<T>(m_data));
    }

    template <typename T>
    std::size_t BandMatrix<T>::lower_bandwidth() const
    {
        return m_data->l;
    }

    template <typename T>
    std::size_t BandMatrix<T>::upper_bandwidth() const
    {
        return m_data->u;
    }
}

#endif // MAFOX_BANDMATRIX_INC