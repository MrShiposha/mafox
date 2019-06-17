
#ifndef MAFOX_UNKNOWNVARIABLE_H
#define MAFOX_UNKNOWNVARIABLE_H

#include "def.h"
#include "amatrix.h"
#include "detail/matrixequationlhs.h"

namespace mafox
{
    enum UnknownVariable
    {
        X = 0,
        Y,
        Z
    };

    template <typename Matrix>
    auto operator*(const Matrix &, UnknownVariable)
        -> detail::MatrixEquationLHS<Matrix>;

}

#endif // MAFOX_UNKNOWNVARIABLE_H