#ifndef MAFOX_AMATRIXEQUATION_INC
#define MAFOX_AMATRIXEQUATION_INC

#include "amatrixequation.h"

namespace mafox
{
    template <typename Matrix, typename Vector>
    AMatrixEquation<Matrix, Vector>::AMatrixEquation(std::shared_ptr<const Matrix> m, std::shared_ptr<const Vector> v)
    : matrix(m), vector(v)
    {}
}

#endif // MAFOX_AMATRIXEQUATION_INC