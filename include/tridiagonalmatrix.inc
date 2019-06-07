#ifndef MAFOX_TRIDIAGONALMATRIX_INC
#define MAFOX_TRIDIAGONALMATRIX_INC

#include "tridiagonalmatrix.h"

namespace mafox
{
    template <typename T>
    TridiagonalMatrix<T>::TridiagonalMatrix(std::size_t size)
    : BandMatrix<T>(size, 1, 1)
    {}
}

#endif // MAFOX_TRIDIAGONALMATRIX_INC