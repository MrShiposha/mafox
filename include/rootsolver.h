#ifndef MAFOX_ROOTSOLVER_H
#define MAFOX_ROOTSOLVER_H

#include "def.h"

namespace mafox
{
    // >>> SKATCH: only for function with one variable <<<
    struct RootSolver
    {
        template <typename T, typename Function, typename Derivative>
        static auto newton
        (
            const Function &function, 
            const Derivative &derivative, 
            T initial_guess, 
            const T &eps = MAFOX_DEFAULT_EPS
        );
    };
}

#endif // MAFOX_ROOTSOLVER_H