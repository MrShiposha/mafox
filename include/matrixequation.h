#ifndef MAFOX_MATRIXEQUATION_H
#define MAFOX_MATRIXEQUATION_H

#include "amatrixequation.h"
#include "tridiagonalmatrix.h"

namespace mafox
{
    template <typename Matrix, typename Vector>
    class MatrixEquation;

    template <typename T, typename Vector>
    class MatrixEquation<TridiagonalMatrix<T>, Vector>
        : public AMatrixEquation<TridiagonalMatrix<T>, Vector>
    {
    public:
        using AMatrixEquation<TridiagonalMatrix<T>, Vector>::AMatrixEquation;

        template <typename LastVariableFormula>
        auto solve(LastVariableFormula) const
            -> typename Vector::template vector_t<std::common_type_t<T, typename Vector::value_type>>;

        auto solve() const
            -> typename Vector::template vector_t<std::common_type_t<T, typename Vector::value_type>>;
    };
}

#endif // MAFOX_MATRIXEQUATION_H