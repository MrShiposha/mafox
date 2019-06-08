#ifndef MAFOX_TRIDIAGONALMATRIX_H
#define MAFOX_TRIDIAGONALMATRIX_H

#include "bandmatrix.h"

namespace mafox
{
    template <typename T>
    class TridiagonalMatrix : public BandMatrix<T>
    {
    public:
        USING_MAFOX_MATRIX_TYPES(BandMatrix<T>);

        TridiagonalMatrix(std::size_t size);

        TridiagonalMatrix(const TridiagonalMatrix &) = default;

        TridiagonalMatrix(TridiagonalMatrix &&) = default;

        virtual ~TridiagonalMatrix() = default;

        TridiagonalMatrix &operator=(const TridiagonalMatrix &) = default;

        TridiagonalMatrix &operator=(TridiagonalMatrix &&) = default;

        pointer lower_diagonal_data();

        const_pointer lower_diagonal_cdata() const;

        pointer upper_diagonal_data();

        const_pointer upper_diagonal_cdata() const;
    };

    template <typename T>
    struct MatrixTraits<TridiagonalMatrix<T>>
    {
        MAFOX_INHERIT_TRAITS(BandMatrix<T>);
    };
}

#endif // MAFOX_TRIDIAGONALMATRIX_H