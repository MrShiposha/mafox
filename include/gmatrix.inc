#ifndef MAFOX_GMATRIX_INC
#define MAFOX_GMATRIX_INC

#include "gmatrix.h"

#include <cstring>

#define MAFOX_SELF GMatrix<T, Allocator>

#define MAFOX_GMATRIX(ReturnType) \
    template <typename T, typename Allocator> \
    ReturnType GMatrix<T, Allocator>

#define INLINE_MAFOX_GMATRIX(ReturnType) \
    template <typename T, typename Allocator> \
    MAFOX_EXPAND(mafox_inline) ReturnType GMatrix<T, Allocator>

namespace mafox
{
    namespace detail
    {
        template <typename T, typename Allocator>
        T *allocate_matrix_data(std::size_t rows, std::size_t cols)
        {
            // TODO: use allocator
           return new T[rows*cols];
        }

        template <typename T, typename Allocator>
        T *reallocate_matrix_data(T *, std::size_t rows, std::size_t cols)
        {
            // TODO: use allocator
           return allocate_matrix_data<T, Allocator>(rows, cols);
        }

        template <typename T, typename Allocator>
        void deallocate_matrix_data(T *addr, std::size_t, std::size_t)
        {
            // TODO: use allocator
            delete []addr;
        }
    }

    MAFOX_GMATRIX()::GMatrix(std::size_t rows, std::size_t cols, T *data, MatrixOrder order)
    : _rows(rows), _cols(cols), t_data(data), order(order)
    {
        assert(rows != 0 && cols != 0);
    }

    MAFOX_GMATRIX()::GMatrix
    (
        std::size_t rows, 
        std::size_t cols, 
        metaxxa::TypeOrRef<const T> initial_value,
        MatrixOrder order
    ): 
    _rows(rows), 
    _cols(cols), 
    t_data(detail::allocate_matrix_data<T, Allocator>(rows, cols)), 
    order(order)
    {
        assert(rows != 0 && cols != 0);

        // TODO: Parallel
        for(std::size_t i = 0, j = 0; i < rows; ++i)
            for(j = 0; j < cols; ++j)
            {
                if(order == ROW_MAJOR)
                    t_data[i*cols + j] = initial_value;
                else
                    t_data[j*cols + i] = initial_value;
            }
    }

    MAFOX_GMATRIX()::GMatrix(const GMatrix &other)
    : _rows(other._rows),
    _cols(other._cols),
    t_data(detail::allocate_matrix_data<T, Allocator>(_rows, _cols)),
    order(other.order)
    {
        const std::size_t size = _rows*_cols*sizeof(T);

        std::memcpy
        (
            static_cast<void *>(t_data),
            static_cast<const void *>(other.t_data),
            size
        );
    }

    MAFOX_GMATRIX()::GMatrix(GMatrix &&other)
    : _rows(other._rows),
    _cols(other._cols),
    t_data(other.t_data),
    order(other.order)
    {
        other.t_data = nullptr;
    }

    MAFOX_GMATRIX()::~GMatrix()
    {
        detail::deallocate_matrix_data<T, Allocator>(t_data, _rows, _cols);
    }

    MAFOX_GMATRIX(MAFOX_SELF &)::operator=(const GMatrix<T> &rhs)
    {
        if(this != &rhs)
        {
            _rows   = rhs._rows;
            _cols   = rhs._cols;
            order  = rhs.order;
            t_data = detail::reallocate_matrix_data<T, Allocator>(t_data, _rows, _cols);

            const std::size_t size = _rows*_cols*sizeof(T);

            memcpy
            (
                static_cast<void *>(t_data),
                static_cast<const void *>(rhs.t_data),
                size
            );
        }

        return *this;
    }

    MAFOX_GMATRIX(MAFOX_SELF &)::operator=(GMatrix<T> &&rhs)
    {
        _rows   = rhs._rows;
        _cols   = rhs._cols;
        order  = rhs.order;
        t_data = rhs.t_data;
        return *this;
    }

    INLINE_MAFOX_GMATRIX(std::size_t)::rows() const
    {
        return _rows;
    }

    INLINE_MAFOX_GMATRIX(std::size_t)::cols() const
    {
        return _cols;
    }

    INLINE_MAFOX_GMATRIX(void)::resize(std::size_t rows, std::size_t cols)
    {
        this->_rows = rows;
        this->_cols = cols;
        t_data = detail::reallocate_matrix_data<T, Allocator>(t_data, _rows, _cols);
    }

    INLINE_MAFOX_GMATRIX(T *)::data()
    {
        return t_data;
    }

    INLINE_MAFOX_GMATRIX(const T *)::data() const
    {
        return MAFOX_NCPTR(this)->data();
    }

    INLINE_MAFOX_GMATRIX(metaxxa::TypeOrRef<T>)::at(std::size_t i, std::size_t j)
    {
        if(order == ROW_MAJOR)
            return t_data[i*_cols + j];
        else
            return t_data[j*_cols + i];
    }

    INLINE_MAFOX_GMATRIX(metaxxa::TypeOrRef<const T>)::at(std::size_t i, std::size_t j) const
    {
        return MAFOX_NCPTR(this)->at(i, j);
    }

    INLINE_MAFOX_GMATRIX(void)::set_at(std::size_t i, std::size_t j, metaxxa::TypeOrRef<T> value)
    {
        if(order == ROW_MAJOR)
            t_data[i*_cols + j] = value;
        else
            t_data[j*_cols + i] = value;
    }
}

#undef MAFOX_GMATRIX
#undef MAFOX_SELF

#endif // MAFOX_GMATRIX_INC