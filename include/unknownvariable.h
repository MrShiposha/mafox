
#ifndef MAFOX_UNKNOWNVARIABLE_H
#define MAFOX_UNKNOWNVARIABLE_H

#include "def.h"
#include "amatrix.h"
#include "homogeneousmatrixequation.h"

namespace mafox
{
    struct UnknownVariable
    {};

    mafox_inline auto unknown_variable();

    template <typename Matrix>
    auto operator*(const Matrix &, UnknownVariable)
        -> HomogeneousMatrixEquation<Matrix>;

}

#endif // MAFOX_UNKNOWNVARIABLE_H