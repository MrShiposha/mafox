#ifndef MAFOX_AMATRIXEQUATION_INC
#define MAFOX_AMATRIXEQUATION_INC

#include "amatrixequation.h"

namespace mafox
{
    template <typename Matrix, typename Vector>
    AMatrixEquation<Matrix, Vector>::AMatrixEquation(const Matrix &m, const Vector &v)
    : matrix(m), vector(v)
    {}

    template <typename Matrix, typename Vector>
    AMatrixEquation<Matrix, Vector>::AMatrixEquation(Matrix &&m, const Vector &v)
    : matrix(std::forward<Matrix>(m)), vector(v)
    {}

    template <typename Matrix, typename Vector>
    AMatrixEquation<Matrix, Vector>::AMatrixEquation(const Matrix &m, Vector &&v)
    : matrix(m), vector(std::forward<Vector>(v))
    {}

    template <typename Matrix, typename Vector>
    AMatrixEquation<Matrix, Vector>::AMatrixEquation(Matrix &&m, Vector &&v)
    : matrix(std::forward<Matrix>(m)), vector(std::forward<Vector>(v))
    {}
}

#endif // MAFOX_AMATRIXEQUATION_INC