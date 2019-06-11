#ifndef MAFOX_BANDMATRIX_INC
#define MAFOX_BANDMATRIX_INC

#include <cstring>

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

    template <typename T, typename MatrixHierarchyEnd>
    BandMatrix<T, MatrixHierarchyEnd>::BandMatrix(std::size_t size, std::size_t lower_bandwidth, std::size_t upper_bandwidth)
    : m_data(band_matrix_data_t<T>::make(size, lower_bandwidth, upper_bandwidth))
    {
        assert(lower_bandwidth < size && upper_bandwidth < size);
    }

    template <typename T, typename MatrixHierarchyEnd>
    BandMatrix<T, MatrixHierarchyEnd>::BandMatrix(const BandMatrix &other)
    : m_data(band_matrix_data_t<T>::make(*other.m_data))
    {}

    template <typename T, typename MatrixHierarchyEnd>
    BandMatrix<T, MatrixHierarchyEnd>::BandMatrix(BandMatrix &&other)
    : m_data(std::move(other.m_data))
    {}

    template <typename T, typename MatrixHierarchyEnd>
    BandMatrix<T, MatrixHierarchyEnd>::BandMatrix(shared_data_t m_data)
    : m_data(m_data)
    {}

    template <typename T, typename MatrixHierarchyEnd>
    BandMatrix<T, MatrixHierarchyEnd>::~BandMatrix()
    {}

    template <typename T, typename MatrixHierarchyEnd>
    BandMatrix<T, MatrixHierarchyEnd> &BandMatrix<T, MatrixHierarchyEnd>::operator=(const BandMatrix &rhs)
    {
        if(this != &rhs)
            *this = std::move(BandMatrix<T, MatrixHierarchyEnd>(rhs));

        return *this;
    }

    template <typename T, typename MatrixHierarchyEnd>
    BandMatrix<T, MatrixHierarchyEnd> &BandMatrix<T, MatrixHierarchyEnd>::operator=(BandMatrix &&rhs)
    {
        if(this != &rhs)
            m_data = std::move(rhs.m_data);

        return *this;
    }

    template <typename T, typename MatrixHierarchyEnd>
    std::size_t BandMatrix<T, MatrixHierarchyEnd>::rows() const
    {
        return m_data->size;
    }

    template <typename T, typename MatrixHierarchyEnd>
    std::size_t BandMatrix<T, MatrixHierarchyEnd>::cols() const
    {
        return m_data->size;
    }

    template <typename T, typename MatrixHierarchyEnd>
    typename BandMatrix<T, MatrixHierarchyEnd>::reference BandMatrix<T, MatrixHierarchyEnd>::element(std::size_t i, std::size_t j)
    {
        assert(i < rows() && j < cols());

        if(m_data->is_zero(i, j))
            MAFOX_FATAL("Accessing non-const zero element of banded matrix");
        else
            return m_data->at(i, j);
    }

    template <typename T, typename MatrixHierarchyEnd>
    typename BandMatrix<T, MatrixHierarchyEnd>::const_reference BandMatrix<T, MatrixHierarchyEnd>::element(std::size_t i, std::size_t j) const
    {
        assert(i < rows() && j < cols());

        return m_data->c_at(i, j);
    }

    template <typename T, typename MatrixHierarchyEnd>
    void BandMatrix<T, MatrixHierarchyEnd>::set_element(std::size_t i, std::size_t j, const_reference value)
    {
        assert(try_set_element(i, j, value));
    }
    
    template <typename T, typename MatrixHierarchyEnd>
    bool BandMatrix<T, MatrixHierarchyEnd>::try_set_element(std::size_t i, std::size_t j, const_reference value)
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

    template <typename T, typename MatrixHierarchyEnd>
    void BandMatrix<T, MatrixHierarchyEnd>::transpose()
    {
        if(lower_bandwidth() == upper_bandwidth())
        {
            for(std::size_t i = 1, diag_index = lower_bandwidth(); i <= diag_index; ++i)
                m_data->arrays[diag_index - i].swap(m_data->arrays[diag_index + i]);
            std::swap(m_data->l, m_data->u);
        }
        else
        {
            BandMatrix<T, MatrixHierarchyEnd> result(m_data->size, m_data->u, m_data->l);
            
            for(std::size_t i = 0, sz = m_data->u+m_data->l; i <= sz; ++i)
                result.m_data->arrays[i] = std::move(m_data->arrays[sz - i]);

            *this = std::move(result);
        }
    }

    template <typename T, typename MatrixHierarchyEnd>
    typename BandMatrix<T, MatrixHierarchyEnd>::template matrix_t<T> BandMatrix<T, MatrixHierarchyEnd>::transposed()
    {
        BandMatrix<T, MatrixHierarchyEnd> result(*this);
        result.transpose();

        return matrix_t<T>(std::move(result));
    }

    template <typename T, typename MatrixHierarchyEnd>
    void BandMatrix<T, MatrixHierarchyEnd>::transpose_rsd()
    {
        long long i = -static_cast<long long>(m_data->l);
        long long u = static_cast<long long>(m_data->u);
        std::size_t col = 0;

        for(; i <= u; ++i, ++col)
            std::reverse(m_data->arrays[col].get(), m_data->arrays[col].get() + m_data->size-std::abs(i));
    }

    template <typename T, typename MatrixHierarchyEnd>
    typename BandMatrix<T, MatrixHierarchyEnd>::template matrix_t<T> BandMatrix<T, MatrixHierarchyEnd>::transposed_rsd()
    {
        BandMatrix<T, MatrixHierarchyEnd> result(*this);
        result.transpose_rsd();

        return matrix_t<T>(std::move(result));
    }

    template <typename T, typename MatrixHierarchyEnd>
    typename BandMatrix<T, MatrixHierarchyEnd>::shared_data_t BandMatrix<T, MatrixHierarchyEnd>::shared_data()
    {
        return m_data;
    }

    template <typename T, typename MatrixHierarchyEnd>
    typename BandMatrix<T, MatrixHierarchyEnd>::const_shared_data_t BandMatrix<T, MatrixHierarchyEnd>::shared_cdata() const
    {
        return m_data;
    }

    template <typename T, typename MatrixHierarchyEnd>
    typename BandMatrix<T, MatrixHierarchyEnd>::template matrix_t<T> BandMatrix<T, MatrixHierarchyEnd>::share()
    {
        return matrix_t<T>(m_data);
    }

    template <typename T, typename MatrixHierarchyEnd>
    std::shared_ptr<IMatrix<T>> BandMatrix<T, MatrixHierarchyEnd>::share_interface()
    {
        return std::shared_ptr<IMatrix<T>>(new BandMatrix<T, MatrixHierarchyEnd>(m_data));
    }

    template <typename T, typename MatrixHierarchyEnd>
    std::shared_ptr<const IMatrix<T>> BandMatrix<T, MatrixHierarchyEnd>::share_interface() const
    {
        return std::shared_ptr<const IMatrix<T>>(new BandMatrix<T, MatrixHierarchyEnd>(m_data));
    }

    template <typename T, typename MatrixHierarchyEnd>
    std::size_t BandMatrix<T, MatrixHierarchyEnd>::lower_bandwidth() const
    {
        return m_data->l;
    }

    template <typename T, typename MatrixHierarchyEnd>
    std::size_t BandMatrix<T, MatrixHierarchyEnd>::upper_bandwidth() const
    {
        return m_data->u;
    }

    template <typename T, typename MatrixHierarchyEnd>
    typename BandMatrix<T, MatrixHierarchyEnd>::pointer BandMatrix<T, MatrixHierarchyEnd>::diagonal_data()
    {
        return m_data->arrays[m_data->l].get();
    }

    template <typename T, typename MatrixHierarchyEnd>
    typename BandMatrix<T, MatrixHierarchyEnd>::const_pointer BandMatrix<T, MatrixHierarchyEnd>::diagonal_cdata() const
    {
        return const_cast<BandMatrix<T, MatrixHierarchyEnd>*>(this)->diagonal_data();
    }

    template <typename T, typename MatrixHierarchyEnd>
    typename BandMatrix<T, MatrixHierarchyEnd>::pointer BandMatrix<T, MatrixHierarchyEnd>::lower_diagonal_data(std::size_t level)
    {
        assert(level < lower_bandwidth());

        return m_data->arrays[m_data->l - (level + 1)].get();
    }

    template <typename T, typename MatrixHierarchyEnd>
    typename BandMatrix<T, MatrixHierarchyEnd>::const_pointer BandMatrix<T, MatrixHierarchyEnd>::lower_diagonal_cdata(std::size_t level) const
    {
        return const_cast<BandMatrix<T, MatrixHierarchyEnd>*>(this)->lower_diagonal_data(level);
    }

    template <typename T, typename MatrixHierarchyEnd>
    typename BandMatrix<T, MatrixHierarchyEnd>::pointer BandMatrix<T, MatrixHierarchyEnd>::upper_diagonal_data(std::size_t level)
    {
        assert(level < upper_bandwidth());

        return m_data->arrays[m_data->l + level + 1].get();
    }

    template <typename T, typename MatrixHierarchyEnd>
    typename BandMatrix<T, MatrixHierarchyEnd>::const_pointer BandMatrix<T, MatrixHierarchyEnd>::upper_diagonal_cdata(std::size_t level) const
    {
        return const_cast<BandMatrix<T, MatrixHierarchyEnd>*>(this)->upper_diagonal_data(level);
    }
}

#endif // MAFOX_BANDMATRIX_INC