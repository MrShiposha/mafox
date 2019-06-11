#ifndef MAFOX_MATRIX_INC
#define MAFOX_MATRIX_INC

#include <cstring>

#include "matrix.h"
#include "util.h"

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

        // TODO: для resize и оператора присваивания обеспечить безопасность в многопоточной среде
        // Важно, чтобы во время, например, транспонирования, умножения и проч. не происходило изменения размера матрицы.
        // См. читатель-писатель 

        std::size_t rows;
        std::size_t cols;
        MatrixOrder order;

        std::unique_ptr<T[]> array; 
    };

    template <typename T, typename MatrixHierarchyEnd>
    Matrix<T, MatrixHierarchyEnd>::Matrix(std::size_t rows, std::size_t cols, MatrixOrder order)
    : m_data(matrix_data_t<T>::make(rows, cols, order))
    {
        std::size_t sz = rows * cols * sizeof(T);

        zero_array(data(), sz);
    }

    // template <typename T, typename MatrixHierarchyEnd>
    // template <typename Iterator>
    // Matrix<T, MatrixHierarchyEnd>::Matrix(Iterator begin, Iterator end)
    // {}

    // template <typename T, typename MatrixHierarchyEnd>
    // Matrix<T, MatrixHierarchyEnd>::Matrix(std::initializer_list<std::initializer_list<T>> list)
    // {}

    template <typename T, typename MatrixHierarchyEnd>
    Matrix<T, MatrixHierarchyEnd>::Matrix(const Matrix &other)
    : m_data(matrix_data_t<T>::make(other.rows(), other.cols(), other.order()))
    {
        memcpy(m_data->array.get(), other.m_data->array.get(), m_data->rows * m_data->cols * sizeof(T));
    }

    template <typename T, typename MatrixHierarchyEnd>
    Matrix<T, MatrixHierarchyEnd>::Matrix(Matrix &&other)
    : m_data(matrix_data_t<T>::make(other.rows(), other.cols(), other.order(), std::move(other.m_data->array)))
    {}

    template <typename T, typename MatrixHierarchyEnd>
    Matrix<T, MatrixHierarchyEnd>::Matrix(shared_data_t m_data)
    : m_data(m_data)
    {}

    template <typename T, typename MatrixHierarchyEnd>
    Matrix<T, MatrixHierarchyEnd>::~Matrix()
    {}

    template <typename T, typename MatrixHierarchyEnd>
    Matrix<T, MatrixHierarchyEnd> &Matrix<T, MatrixHierarchyEnd>::operator=(const Matrix &rhs)
    {
        if(this != &rhs)
            *this = std::move(Matrix<T, MatrixHierarchyEnd>(rhs));
        
        return *this;
    }

    template <typename T, typename MatrixHierarchyEnd>
    Matrix<T, MatrixHierarchyEnd> &Matrix<T, MatrixHierarchyEnd>::operator=(Matrix &&rhs)
    {
        if(this != &rhs)
            m_data = std::move(rhs.m_data);
        
        return *this;
    }

    template <typename T, typename MatrixHierarchyEnd>
    mafox_inline std::size_t Matrix<T, MatrixHierarchyEnd>::rows() const
    {
        return m_data->rows;
    }

    template <typename T, typename MatrixHierarchyEnd>
    mafox_inline std::size_t Matrix<T, MatrixHierarchyEnd>::cols() const
    {
        return m_data->cols;
    }

    template <typename T, typename MatrixHierarchyEnd>
    typename Matrix<T, MatrixHierarchyEnd>::reference Matrix<T, MatrixHierarchyEnd>::element(std::size_t i, std::size_t j)
    {
        assert(i < rows() && j < cols());

        if(order() == ROW_MAJOR)
            return data()[i*cols() + j];
        else if(order() == COL_MAJOR)
            return data()[j*rows() + i];
        else
            MAFOX_FATAL("Unknown matrix order");
    }

    template <typename T, typename MatrixHierarchyEnd>
    typename Matrix<T, MatrixHierarchyEnd>::const_reference Matrix<T, MatrixHierarchyEnd>::element(std::size_t i, std::size_t j) const
    {
        return const_cast<Matrix<T, MatrixHierarchyEnd>*>(this)->element(i, j);
    }

    template <typename T, typename MatrixHierarchyEnd>
    void Matrix<T, MatrixHierarchyEnd>::set_element(std::size_t i, std::size_t j, const_reference value)
    {
        element(i, j) = value;
    }

    template <typename T, typename MatrixHierarchyEnd>
    void Matrix<T, MatrixHierarchyEnd>::transpose()
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

    template <typename T, typename MatrixHierarchyEnd>
    typename Matrix<T, MatrixHierarchyEnd>::template matrix_t<T> Matrix<T, MatrixHierarchyEnd>::transposed()
    {
        std::size_t r = rows(), c = cols();
        Matrix<T, MatrixHierarchyEnd> result(c, r);

        for(std::size_t i = 0, j = 0; i < r; ++i)
            for(j = 0; j < c; ++j)
                result.set_element(j, i, element(i, j));

        return matrix_t<T>(std::move(result));
    }

    template <typename T, typename MatrixHierarchyEnd>
    void Matrix<T, MatrixHierarchyEnd>::transpose_rsd()
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

    template <typename T, typename MatrixHierarchyEnd>
    typename Matrix<T, MatrixHierarchyEnd>::template matrix_t<T> Matrix<T, MatrixHierarchyEnd>::transposed_rsd()
    {
        std::size_t r = rows(), c = cols();
        Matrix<T, MatrixHierarchyEnd> result(c, r);

        for(std::size_t i = 0, j = 0; i < r; ++i)
            for(j = 0; j < c; ++j)
                result.set_element(c - j - 1, r - i - 1, element(i, j));

        return matrix_t<T>(std::move(result));
    }

    template <typename T, typename MatrixHierarchyEnd>
    typename Matrix<T, MatrixHierarchyEnd>::shared_data_t Matrix<T, MatrixHierarchyEnd>::shared_data()
    {
        return m_data;
    }

    template <typename T, typename MatrixHierarchyEnd>
    typename Matrix<T, MatrixHierarchyEnd>::const_shared_data_t Matrix<T, MatrixHierarchyEnd>::shared_cdata() const
    {
        return m_data;
    }

    template <typename T, typename MatrixHierarchyEnd>
    typename Matrix<T, MatrixHierarchyEnd>::template matrix_t<T> Matrix<T, MatrixHierarchyEnd>::share()
    {
        return matrix_t<T>(m_data);
    }

    template <typename T, typename MatrixHierarchyEnd>
    std::shared_ptr<IMatrix<T>> Matrix<T, MatrixHierarchyEnd>::share_interface()
    {
        return std::shared_ptr<Matrix<T, MatrixHierarchyEnd>>(new Matrix<T, MatrixHierarchyEnd>(m_data));
    }

    template <typename T, typename MatrixHierarchyEnd>
    std::shared_ptr<const IMatrix<T>> Matrix<T, MatrixHierarchyEnd>::share_interface() const
    {
        return std::shared_ptr<Matrix<T, MatrixHierarchyEnd>>(new Matrix<T, MatrixHierarchyEnd>(m_data));
    }

    template <typename T, typename MatrixHierarchyEnd>
    typename Matrix<T, MatrixHierarchyEnd>::pointer Matrix<T, MatrixHierarchyEnd>::data()
    {
        return m_data->array.get();
    }

    template <typename T, typename MatrixHierarchyEnd>
    typename Matrix<T, MatrixHierarchyEnd>::const_pointer Matrix<T, MatrixHierarchyEnd>::cdata() const
    {
        return m_data->array.get();
    }

    template <typename T, typename MatrixHierarchyEnd>
    MatrixOrder Matrix<T, MatrixHierarchyEnd>::order() const
    {
        return m_data->order;
    }

    template <typename T, typename MatrixHierarchyEnd>
    void Matrix<T, MatrixHierarchyEnd>::set_order(MatrixOrder order)
    {
        if(m_data->order != order)
        {
            std::size_t r = rows(), c = cols();
            Matrix<T, MatrixHierarchyEnd> reordered(r, c, order);
            for(std::size_t i = 0, j = 0; i < r; ++i)
                for(j = 0; j < c; ++j)
                    reordered.set_element(i, j, element(i, j));

            *this = std::move(reordered);
        }
    }
}

#endif // MAFOX_MATRIX_INC