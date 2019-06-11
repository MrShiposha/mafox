#ifndef MAFOX_HOMOGENEOUSMATRIXEQUATION_H
#define MAFOX_HOMOGENEOUSMATRIXEQUATION_H

#include "amatrix.h"
#include "matrixequation.h"

namespace mafox
{
    template <typename Matrix>
    class HomogeneousMatrixEquation
    {
    public:
        static_assert(is_matrix<Matrix>());

        HomogeneousMatrixEquation(const Matrix &);

        HomogeneousMatrixEquation() = delete;

        HomogeneousMatrixEquation(const HomogeneousMatrixEquation &) = default;

        HomogeneousMatrixEquation(HomogeneousMatrixEquation &&) = default;

        ~HomogeneousMatrixEquation() = default;

        HomogeneousMatrixEquation &operator=(const HomogeneousMatrixEquation &) = delete;

        HomogeneousMatrixEquation &operator=(HomogeneousMatrixEquation &&) = delete;

        // TODO for lvalue eq and rvalue vector
        template <typename Vector>
        auto operator=(const Vector &) const &&
            -> MatrixEquation<Matrix, Vector>;

        // TODO auto solve();

    private:
        std::shared_ptr<const Matrix> matrix;
    };
}

#endif // MAFOX_HOMOGENEOUSMATRIXEQUATION_H