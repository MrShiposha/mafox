
#ifndef MAFOX_UNKNOWNVARIABLE_INC
#define MAFOX_UNKNOWNVARIABLE_INC

#include "unknownvariable.h"

namespace mafox
{
    mafox_inline auto unknown_variable() 
    { 
        return UnknownVariable(); 
    }

    template <typename Matrix>
    auto operator*(const Matrix &matrix, UnknownVariable)
        -> HomogeneousMatrixEquation<Matrix>
    {
        return HomogeneousMatrixEquation<Matrix>(matrix);
    }
}

#endif // MAFOX_UNKNOWNVARIABLE_INC