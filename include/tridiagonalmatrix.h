#ifndef MAFOX_TRIDIAGONALMATRIX_H
#define MAFOX_TRIDIAGONALMATRIX_H

#include "bandmatrix.h"

namespace mafox
{
    template <typename T, typename MatrixHierarchyEnd = This>
    class TridiagonalMatrix : public MatrixExtender<BandMatrix, TridiagonalMatrix<T>, MatrixHierarchyEnd>
    {
    public:
        using base_matrix_t = MatrixExtender<::mafox::BandMatrix, TridiagonalMatrix<T>, MatrixHierarchyEnd>;
        USING_MAFOX_MATRIX_TYPES(TridiagonalMatrix);

        TridiagonalMatrix(std::size_t size);

        TridiagonalMatrix(base_matrix_t &&);

        TridiagonalMatrix(const TridiagonalMatrix &) = default;

        TridiagonalMatrix(TridiagonalMatrix &&) = default;

        virtual ~TridiagonalMatrix() = default;

        TridiagonalMatrix &operator=(const TridiagonalMatrix &) = default;

        TridiagonalMatrix &operator=(TridiagonalMatrix &&) = default;

        pointer lower_diagonal_data();

        const_pointer lower_diagonal_cdata() const;

        pointer upper_diagonal_data();

        const_pointer upper_diagonal_cdata() const;

    protected:
        friend base_matrix_t;

        TridiagonalMatrix(shared_data_t);
    };

    template <typename T, typename MatrixHierarchyEnd>
    struct MatrixTraits<TridiagonalMatrix<T, MatrixHierarchyEnd>>
    {
        MAFOX_INHERIT_TRAITS(TridiagonalMatrix, T, BandMatrix);
    };
}

#endif // MAFOX_TRIDIAGONALMATRIX_H