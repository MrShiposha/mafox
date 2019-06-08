#ifndef MAFOX_MATRIXEQUATION_INC
#define MAFOX_MATRIXEQUATION_INC

#include "matrixequation.h"
#include "tridiagonalmatrix.inc"

namespace mafox
{
    template <typename T, typename Vector>
    template <typename LastVariableFormula>
    auto MatrixEquation<TridiagonalMatrix<T>, Vector>::solve(LastVariableFormula formula) const
        -> typename Vector::template vector_t<std::common_type_t<T, typename Vector::value_type>>
    {
        using R = std::common_type_t<T, typename Vector::value_type>;
        using Result = typename Vector::template vector_t<R>;

        const std::size_t N = this->vector.dimension();

        std::unique_ptr<R[]> xi(new R[N-1]);
        std::unique_ptr<R[]> eta(new R[N-1]);

        auto *a = this->matrix.lower_diagonal_cdata();
        auto *b = this->matrix.diagonal_cdata();
        auto *c = this->matrix.upper_diagonal_cdata();

        xi[0]  = -c[0] / b[0];
        eta[0] = this->vector(0) / b[0];

        R div = 0;
        for(std::size_t i = 1, j = 0; i < N-1; ++i, ++j)
        {
            div = (b[i] + a[j]*xi[j]);
            assert(div != 0);

            xi[i] = -c[i] / div;
            eta[i] = (this->vector(i) - a[j]*eta[j]) / div;
        }

        Result result(N);

        result.set_element(N-1, formula(xi[N-2], eta[N-2], a[N-2], b[N-1], c[N-2], this->vector(N-1)));
        for(std::size_t i = N-1, j = N-2; i > 0; --i, --j)
            result.set_element(j, xi[j]*result(i) + eta[j]);

        return result;
    }

    template <typename T, typename Vector>
    auto MatrixEquation<TridiagonalMatrix<T>, Vector>::solve() const
        -> typename Vector::template vector_t<std::common_type_t<T, typename Vector::value_type>>
    {
        return solve
        (
            []
            (
                const auto &xi, 
                const auto &eta, 
                const auto &a, 
                const auto &b, 
                const auto &c, 
                const auto &d
            ) 
            { 
                return (d - a*eta) / (b + a*xi);
            }
        );
    }
}

#endif // MAFOX_MATRIXEQUATION_INC