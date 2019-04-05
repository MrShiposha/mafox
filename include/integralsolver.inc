#ifndef MAFOX_INTEGRALSOLVER_INC
#define MAFOX_INTEGRALSOLVER_INC

#include "integralsolver.h"

#include "legendre.inc"

namespace mafox
{   
    template <typename XT, typename Function, typename NodesCount, typename Eps>
    auto IntegralSolver::gaussian
    (
        const Function &f, 
        metaxxa::TypeOrRef<const XT> from,
        metaxxa::TypeOrRef<const XT> to,
        metaxxa::TypeOrRef<const NodesCount> nodes, 
        metaxxa::TypeOrRef<const Eps> eps   
    )
    {
        
        using ResultT = double; // TODO Function::Result instead double

        LegendrePolynomial<XT> polynomial(nodes);

        // For variable replacing
        auto k      = (to - from) / 2;
        auto offset = (to + from) / 2;

        auto roots = polynomial.roots(eps);

        ResultT result = 0.0;
        ResultT weight = 0.0;
        ResultT derivative = 0.0;
        for(std::size_t i = 0; i < nodes; ++i)
        {
            derivative = polynomial.derivative(roots[i]);
            weight = 2.0/((1 - roots[i]*roots[i]) * derivative*derivative);

            result += weight * f(k*roots[i] + offset);
        }

        return k*result;
    }
}

#endif // MAFOX_INTEGRALSOLVER_INC