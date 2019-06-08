
#ifndef MAFOX_UNKNOWNVARIABLE_H
#define MAFOX_UNKNOWNVARIABLE_H

#include "def.h"
#include "amatrix.h"
#include "homogeneousmatrixequation.h"

namespace mafox
{
    struct UnknownVariable
    {};

    static inline constexpr UnknownVariable X; 

    template <typename Matrix>
    auto operator*(const Matrix &, UnknownVariable)
        -> HomogeneousMatrixEquation<Matrix>;

}

#endif // MAFOX_UNKNOWNVARIABLE_H