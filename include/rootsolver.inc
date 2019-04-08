#ifndef MAFOX_ROOTSOLVER_INC
#define MAFOX_ROOTSOLVER_INC

#include "rootsolver.h"

#include <cmath>

namespace mafox
{
    template <typename T, typename Function, typename Derivative>
    auto RootSolver::newton
    (
        const Function &function, 
        const Derivative &derivative, 
        T x0, 
        const T &eps
    )
    {
        T x1 = x0 - function(x0) / derivative(x0);
        while(std::abs(x1 - x0) >= eps)
        {
            x0 = x1;
            x1 = x1 - function(x1) / derivative(x1);
        }

        return x1;
    }

    template 
    <
        typename T,
        typename IntT,
        typename Cache
    >
    auto RootSolver::newton
    (
        const LegendrePolynomial<T, IntT, Cache> &polynomial,
        T x0,
        const T &eps
    )
    {
        auto [p_n, p_n1] = polynomial.pair(x0);
        auto dp_n = polynomial.derivative(x0, p_n, p_n1);

        T x1 = x0 - p_n / dp_n;
        while(std::abs(x1 - x0) >= eps)
        {
            std::tie(p_n, p_n1) = polynomial.pair(x1);
            dp_n = polynomial.derivative(x1, p_n, p_n1);

            x0 = x1;
            x1 = x1 - p_n / dp_n;
        }

        return x1;
    }
}

#endif // MAFOX_ROOTSOLVER_INC