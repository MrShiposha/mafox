#ifndef MAFOX_INTEGRALSOLVER_H
#define MAFOX_INTEGRALSOLVER_H

#include "def.h"

namespace mafox
{
    // >>> SKATCH: only for one variable <<<
    struct IntegralSolver
    {
        template 
        <
            typename XT,
            typename Function, 
            typename NodesCount = unsigned int, 
            typename Eps = double
            // TODO: typename LegendreRootsContainer = std::vector<typename metaxxa::Function<Function>::Result>
        >
        static auto gaussian
        (
            const Function &f,
            metaxxa::TypeOrRef<const XT> from,
            metaxxa::TypeOrRef<const XT> to,
            metaxxa::TypeOrRef<const NodesCount>, 
            metaxxa::TypeOrRef<const Eps> = MAFOX_DEFAULT_EPS
        );

        // TODO: with cache
    };

}

#endif // MAFOX_INTEGRALSOLVER_H