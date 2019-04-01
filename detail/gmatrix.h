#ifndef MAFOX_DETAIL_GMATRIX_H
#define MAFOX_DETAIL_GMATRIX_H

#include <cstddef>
#include <cstdint>
#include <memory>

#include "matrixalllocator.h"
#include "def.h"
#include "inline.h"

#include "../orientation.h"
#include "../vectormutex.h"

namespace mafox::detail
{
    template 
    <
        typename T, 
        typename Allocator  = MatrixAllocator<T>,
        typename Mutex      = typename VectorMutex<T>::Type,
        typename MutexIndex = Byte
    >
    struct GMatrix
    {
        GMatrix(std::size_t rows, std::size_t cols, T *data);

        GMatrix(std::size_t rows, std::size_t cols, const T &initial_value = T());

        GMatrix(const GMatrix &);

        GMatrix(GMatrix &&);

        GMatrix &operator=(const GMatrix<T> &);

        GMatrix &operator=(GMatrix<T> &&);

        mafox_inline void resize(std::size_t rows, std::size_t cols);

        mafox_inline T *tdata();

        mafox_inline const T *tdata() const;

        mafox_inline T &at(std::size_t i, std::size_t j);

        mafox_inline const T &at(std::size_t i, std::size_t j) const;

        mafox_inline Mutex *mutex(size_t i, size_t j) const;

        mafox_inline void lock(std::size_t i, std::size_t j, std::size_t vector_length, Orientation) const;

        mafox_inline void unlock(std::size_t i, std::size_t j) const;

        std::size_t rows, cols;
        Byte *data;
    };
}

#endif // MAFOX_DETAIL_GMATRIX_H