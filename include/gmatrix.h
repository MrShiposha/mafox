#ifndef MAFOX_GMATRIX_H
#define MAFOX_GMATRIX_H

#include <cstddef>
#include <cstdint>
#include <memory>

#include "def.h"

#include "orientation.h"

namespace mafox
{
    enum MatrixOrder
    {
        ROW_MAJOR,
        COLUMN_MAJOR
    };

    template 
    <
        typename T,
        typename Allocator = std::allocator<T>
    >
    struct GMatrix
    {
        GMatrix(std::size_t rows, std::size_t cols, T *data, MatrixOrder);

        GMatrix
        (
            std::size_t rows, 
            std::size_t cols, 
            metaxxa::TypeOrRef<const T> initial_value = T(), 
            MatrixOrder = ROW_MAJOR
        );

        GMatrix(const GMatrix &);

        GMatrix(GMatrix &&);

        ~GMatrix();

        GMatrix &operator=(const GMatrix<T> &);

        GMatrix &operator=(GMatrix<T> &&);

        mafox_inline std::size_t rows() const;

        mafox_inline std::size_t cols() const;

        mafox_inline void resize(std::size_t rows, std::size_t cols);

        mafox_inline T *data();

        mafox_inline const T *data() const;

        mafox_inline metaxxa::TypeOrRef<T> at(std::size_t i, std::size_t j);

        mafox_inline metaxxa::TypeOrRef<const T> at(std::size_t i, std::size_t j) const;

        mafox_inline void set_at(std::size_t i, std::size_t j, metaxxa::TypeOrRef<T> value);

        std::size_t _rows, _cols;
        T *t_data;
        MatrixOrder order;
    };
}

#endif // MAFOX_DETAIL_GMATRIX_H