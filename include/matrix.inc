#ifndef MAFOX_MATRIX_INC
#define MAFOX_MATRIX_INC

#include "matrix.h"

namespace mafox
{
    template <typename T>
    class matrix_data_t
    {
        matrix_data_t(std::size_t rows, std::size_t cols, MatrixOrder order, std::unique_ptr<T[]> &&array)
        : rows(rows), cols(cols), order(order), array(std::forward<std::unique_ptr<T[]>>(array))
        {}

        matrix_data_t(std::size_t rows, std::size_t cols, MatrixOrder order)
        : rows(rows), cols(cols), order(order), array(new T[rows*cols])
        {}
        
    public:
        static auto make(std::size_t rows, std::size_t cols, MatrixOrder order, std::unique_ptr<T[]> &&array)
        {
            return std::shared_ptr<matrix_data_t<T>>
            (
                new matrix_data_t<T>
                (
                    rows, 
                    cols, 
                    order, 
                    std::forward<std::unique_ptr<T[]>>(array)
                )
            );
        }

        static auto make(std::size_t rows, std::size_t cols, MatrixOrder order)
        {
            return std::shared_ptr<matrix_data_t<T>>
            (
                new matrix_data_t<T>
                (
                    rows, 
                    cols, 
                    order
                )
            );
        }

        std::size_t rows;
        std::size_t cols;
        MatrixOrder order;

        std::unique_ptr<T[]> array; 
    };

    template <typename T>
    Matrix<T>::Matrix(std::size_t rows, std::size_t cols, MatrixOrder order)
    : m_data(matrix_data_t<T>::make(rows, cols, order))
    {
        std::size_t sz = rows * cols * sizeof(T);
        memset_s(data(), sz, 0, sz);
    }

    // template <typename T>
    // template <typename Iterator>
    // Matrix<T>::Matrix(Iterator begin, Iterator end)
    // {}

    // template <typename T>
    // Matrix<T>::Matrix(std::initializer_list<std::initializer_list<T>> list)
    // {}

    template <typename T>
    Matrix<T>::Matrix(const Matrix &other)
    : m_data(matrix_data_t<T>::make(other.rows(), other.cols(), other.order()))
    {
        memcpy(m_data->array.get(), other.m_data->array.get(), m_data->rows * m_data->cols * sizeof(T));
    }

    template <typename T>
    Matrix<T>::Matrix(Matrix &&other)
    : m_data(matrix_data_t<T>::make(other.rows(), other.cols(), other.order(), std::move(other.m_data->array)))
    {}

    template <typename T>
    Matrix<T>::~Matrix()
    {}

    template <typename T>
    Matrix<T> &Matrix<T>::operator=(const Matrix &rhs)
    {
        if(this != &rhs)
            *this = std::move(Matrix<T>(rhs));
        
        return *this;
    }

    template <typename T>
    Matrix<T> &Matrix<T>::operator=(Matrix &&rhs)
    {
        if(this != &rhs)
        {
            m_data->rows  = rhs.m_data->rows;
            m_data->cols  = rhs.m_data->cols;
            m_data->array = std::move(rhs.m_data->array);
        }
        
        return *this;
    }

    template <typename T>
    mafox_inline std::size_t Matrix<T>::rows() const
    {
        return m_data->rows;
    }

    template <typename T>
    mafox_inline std::size_t Matrix<T>::cols() const
    {
        return m_data->cols;
    }

    template <typename T>
    typename Matrix<T>::reference Matrix<T>::element(std::size_t i, std::size_t j)
    {
        assert(i < rows() && j < cols());

        if(order() == ROW_MAJOR)
            return data()[i*cols() + j];
        else if(order() == COL_MAJOR)
            return data()[j*rows() + i];
        else
            MAFOX_FATAL("Unknown matrix order");
    }

    template <typename T>
    typename Matrix<T>::const_reference Matrix<T>::element(std::size_t i, std::size_t j) const
    {
        return const_cast<Matrix<T>*>(this)->element(i, j);
    }

    template <typename T>
    void Matrix<T>::set_element(std::size_t i, std::size_t j, const_reference value)
    {
        element(i, j) = value;
    }

    template <typename T>
    void Matrix<T>::transpose()
    {
        if(this->is_square())
        {
            std::size_t sz = rows();
            for(std::size_t i = 1, j = 0, diff = 0; i < sz; ++i)
                for(j = i; j > 0; --j)
                {
                    diff = i-j;
                    std::swap(element(i, diff), element(diff, i));
                }

            std::swap(m_data->rows, m_data->cols);        
        }
        else
            *this = std::move(transposed());
    }

    template <typename T>
    Matrix<T> Matrix<T>::transposed()
    {
        std::size_t r = rows(), c = cols();
        Matrix<T> result(c, r);

        for(std::size_t i = 0, j = 0; i < r; ++i)
            for(j = 0; j < c; ++j)
                result.set_element(j, i, element(i, j));

        return result;
    }

    template <typename T>
    void Matrix<T>::transpose_rsd()
    {
        if(this->is_square())
        {
            std::size_t sz = rows();
            for(std::size_t i = 0, j = 0, diff = 0; i < sz; ++i)
                for(diff = sz-i-1, j = diff; j > 0; --j)
                {
                    std::swap(element(diff-j, i), element(diff, i+j));
                }

            std::swap(m_data->rows, m_data->cols);        
        }
        else
            *this = std::move(transposed_rsd());
    }

    template <typename T>
    Matrix<T> Matrix<T>::transposed_rsd()
    {
        std::size_t r = rows(), c = cols();
        Matrix<T> result(c, r);

        for(std::size_t i = 0, j = 0; i < r; ++i)
            for(j = 0; j < c; ++j)
                result.set_element(c - j - 1, r - i - 1, element(i, j));

        return result;
    }

    template <typename T>
    typename Matrix<T>::shared_data_t Matrix<T>::shared_data()
    {
        return m_data;
    }

    template <typename T>
    typename Matrix<T>::const_shared_data_t Matrix<T>::shared_cdata() const
    {
        return m_data;
    }

    template <typename T>
    typename Matrix<T>::pointer Matrix<T>::data()
    {
        return m_data->array.get();
    }

    template <typename T>
    typename Matrix<T>::const_pointer Matrix<T>::cdata() const
    {
        return m_data->array.get();
    }

    template <typename T>
    MatrixOrder Matrix<T>::order() const
    {
        return m_data->order;
    }

    template <typename T>
    void Matrix<T>::set_order(MatrixOrder order)
    {
        if(m_data->order != order)
        {
            this->transpose();
            m_data->order = order;
        }
    }
}

#endif // MAFOX_MATRIX_INC