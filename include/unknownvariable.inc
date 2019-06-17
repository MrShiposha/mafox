
#ifndef MAFOX_UNKNOWNVARIABLE_INC
#define MAFOX_UNKNOWNVARIABLE_INC

#include "unknownvariable.h"

namespace mafox
{
    template <typename Matrix>
    auto operator*(const Matrix &matrix, UnknownVariable)
        -> detail::MatrixEquationLHS<Matrix>
    {
        return detail::MatrixEquationLHS<Matrix>(matrix);
    }
}

#endif // MAFOX_UNKNOWNVARIABLE_INC