#ifndef MAFOX_TRIDIAGONALMATRIX_INC
#define MAFOX_TRIDIAGONALMATRIX_INC

#include "tridiagonalmatrix.h"

namespace mafox
{
    template <typename T, typename MatrixHierarchyEnd>
    TridiagonalMatrix<T, MatrixHierarchyEnd>::TridiagonalMatrix(std::size_t size)
    : base_matrix_t(size, 1, 1)
    {}

    template <typename T, typename MatrixHierarchyEnd>
    TridiagonalMatrix<T, MatrixHierarchyEnd>::TridiagonalMatrix(base_matrix_t &&other)
    : base_matrix_t(std::forward<base_matrix_t>(other))
    {}

    template <typename T, typename MatrixHierarchyEnd>
    TridiagonalMatrix<T, MatrixHierarchyEnd>::TridiagonalMatrix(shared_data_t m_data)
    : base_matrix_t(m_data)
    {}

    template <typename T, typename MatrixHierarchyEnd>
    typename TridiagonalMatrix<T, MatrixHierarchyEnd>::pointer TridiagonalMatrix<T, MatrixHierarchyEnd>::lower_diagonal_data()
    {
        return base_matrix_t::lower_diagonal_data(0);
    }

    template <typename T, typename MatrixHierarchyEnd>
    typename TridiagonalMatrix<T, MatrixHierarchyEnd>::const_pointer TridiagonalMatrix<T, MatrixHierarchyEnd>::lower_diagonal_cdata() const
    {
        return base_matrix_t::lower_diagonal_cdata(0);
    }

    template <typename T, typename MatrixHierarchyEnd>
    typename TridiagonalMatrix<T, MatrixHierarchyEnd>::pointer TridiagonalMatrix<T, MatrixHierarchyEnd>::upper_diagonal_data()
    {
        return base_matrix_t::upper_diagonal_data(0);
    }

    template <typename T, typename MatrixHierarchyEnd>
    typename TridiagonalMatrix<T, MatrixHierarchyEnd>::const_pointer TridiagonalMatrix<T, MatrixHierarchyEnd>::upper_diagonal_cdata() const
    {
        return base_matrix_t::upper_diagonal_cdata(0);
    }
}

#endif // MAFOX_TRIDIAGONALMATRIX_INC