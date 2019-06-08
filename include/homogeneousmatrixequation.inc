#ifndef MAFOX_HOMOGENEOUSMATRIXEQUATION_INC
#define MAFOX_HOMOGENEOUSMATRIXEQUATION_INC

#include "homogeneousmatrixequation.h"
#include "matrixequation.inc"

namespace mafox
{
    template <typename Matrix>
    HomogeneousMatrixEquation<Matrix>::HomogeneousMatrixEquation(const Matrix &matrix)
    : matrix(matrix)
    {
        assert(matrix.is_square());
    }

    template <typename Matrix>
    HomogeneousMatrixEquation<Matrix>::HomogeneousMatrixEquation(Matrix &&matrix)
    : matrix(std::forward<Matrix>(matrix))
    {
        assert(matrix.is_square());
    }

    template <typename Matrix>
    template <typename Vector>
    auto HomogeneousMatrixEquation<Matrix>::operator=(const Vector &vector) const &&
        -> MatrixEquation<Matrix, Vector>
    {
        assert(vector.dimension() == matrix.rows());

        return MatrixEquation<Matrix, Vector>(std::move(matrix), vector);
    }
}

#endif // MAFOX_HOMOGENEOUSMATRIXEQUATION_INC