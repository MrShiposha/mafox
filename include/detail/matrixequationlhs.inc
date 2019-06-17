#ifndef MAFOX_MATRIXEQUATIONLHS_INC
#define MAFOX_MATRIXEQUATIONLHS_INC

#include "matrixequationlhs.h"
#include "../matrixequation.inc"
#include "../unknownvariable.h"

namespace mafox::detail
{
    template <typename Matrix>
    MatrixEquationLHS<Matrix>::MatrixEquationLHS(const Matrix &matrix)
    : matrix(matrix.share_ptr())
    {
        assert(matrix.is_square());
    }

    template <typename Matrix>
    template <typename Vector>
    auto MatrixEquationLHS<Matrix>::operator=(const Vector &vector) const
        -> std::enable_if_t<is_vector<Vector>(), MatrixEquation<Matrix, Vector>>
    {
        assert(vector.dimension() == matrix->rows());

        return MatrixEquation<Matrix, Vector>(matrix, vector.share_ptr());
    }

    template <typename Matrix>
    template <typename Value>
    auto MatrixEquationLHS<Matrix>::operator=(const Value &value) const
        -> std::enable_if_t<!is_matrix<Value>(), MatrixEquation<Matrix, mafox::Vector<Value>>>
    {
        mafox::Vector<Value> rhs_vector(matrix->rows(), value);
        return (*matrix*X = rhs_vector);
    }
}

#endif // MAFOX_MATRIXEQUATIONLHS_INC