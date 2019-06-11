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
        static_assert(is_matrix<Matrix>());
        static_assert(is_vector<Vector>());

        AMatrixEquation(std::shared_ptr<const Matrix>, std::shared_ptr<const Vector>);

        AMatrixEquation() = delete;

        AMatrixEquation(const AMatrixEquation &) = default;

        AMatrixEquation(AMatrixEquation &&) = default;

        virtual ~AMatrixEquation() = default;

        AMatrixEquation &operator=(const AMatrixEquation &) = delete;

        AMatrixEquation &operator=(AMatrixEquation &&) = delete;

    protected:
        std::shared_ptr<const Matrix> matrix;
        std::shared_ptr<const Vector> vector;
    };
}

#endif // MAFOX_AMATRIXEQUATION_H