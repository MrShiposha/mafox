#ifndef MAFOX_MATRIXEQUATIONLHS_H
#define MAFOX_MATRIXEQUATIONLHS_H

#include "../amatrix.h"
#include "../vector.h"
#include "../matrixequation.h"

namespace mafox::detail
{
    template <typename Matrix>
    class MatrixEquationLHS
    {
    public:
        static_assert(is_matrix<Matrix>());

        MatrixEquationLHS(const Matrix &);

        MatrixEquationLHS() = delete;

        MatrixEquationLHS(const MatrixEquationLHS &) = default;

        MatrixEquationLHS(MatrixEquationLHS &&) = default;

        ~MatrixEquationLHS() = default;

        MatrixEquationLHS &operator=(const MatrixEquationLHS &) = delete;

        MatrixEquationLHS &operator=(MatrixEquationLHS &&) = delete;

        template <typename Vector>
        auto operator=(const Vector &) const
            -> std::enable_if_t<is_vector<Vector>(), MatrixEquation<Matrix, Vector>>;

        template <typename Value>
        auto operator=(const Value &) const
            -> std::enable_if_t<!is_matrix<Value>(), MatrixEquation<Matrix, mafox::Vector<Value>>>;

    private:
        std::shared_ptr<const Matrix> matrix;
    };
}

#endif // MAFOX_MATRIXEQUATIONLHS_H