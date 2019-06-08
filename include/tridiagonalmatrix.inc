#ifndef MAFOX_TRIDIAGONALMATRIX_INC
#define MAFOX_TRIDIAGONALMATRIX_INC

#include "tridiagonalmatrix.h"

namespace mafox
{
    template <typename T>
    TridiagonalMatrix<T>::TridiagonalMatrix(std::size_t size)
    : BandMatrix<T>(size, 1, 1)
    {}

    template <typename T>
    typename TridiagonalMatrix<T>::pointer TridiagonalMatrix<T>::lower_diagonal_data()
    {
        return BandMatrix<T>::lower_diagonal_data(0);
    }

    template <typename T>
    typename TridiagonalMatrix<T>::const_pointer TridiagonalMatrix<T>::lower_diagonal_cdata() const
    {
        return BandMatrix<T>::lower_diagonal_cdata(0);
    }

    template <typename T>
    typename TridiagonalMatrix<T>::pointer TridiagonalMatrix<T>::upper_diagonal_data()
    {
        return BandMatrix<T>::upper_diagonal_data(0);
    }

    template <typename T>
    typename TridiagonalMatrix<T>::const_pointer TridiagonalMatrix<T>::upper_diagonal_cdata() const
    {
        return BandMatrix<T>::upper_diagonal_cdata(0);
    }
}

#endif // MAFOX_TRIDIAGONALMATRIX_INC