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
}

#endif // MAFOX_ROOTSOLVER_INC