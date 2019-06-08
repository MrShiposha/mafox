#ifndef MAFOX_AMATRIXEQUATION_H
#define MAFOX_AMATRIXEQUATION_H

#include "amatrix.h"
#include "avector.h"

namespace mafox
{
    template <typename Matrix, typename Vector>
    class AMatrixEquation
    {
    public:
        static_assert(std::is_base_of_v<MatrixTag, Matrix>);
        static_assert(std::is_base_of_v<VectorTag, Vector>);

        AMatrixEquation(const Matrix &, const Vector &);

        AMatrixEquation(Matrix &&, const Vector &);

        AMatrixEquation(const Matrix &, Vector &&);

        AMatrixEquation(Matrix &&, Vector &&);

        AMatrixEquation() = delete;

        AMatrixEquation(const AMatrixEquation &) = default;

        AMatrixEquation(AMatrixEquation &&) = default;

        virtual ~AMatrixEquation() = default;

        AMatrixEquation &operator=(const AMatrixEquation &) = delete;

        AMatrixEquation &operator=(AMatrixEquation &&) = delete;

    protected:
        Matrix matrix;
        Vector vector;
    };
}

#endif // MAFOX_AMATRIXEQUATION_H