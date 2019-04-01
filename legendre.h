#ifndef MAFOX_LEGENDRE_H
#define MAFOX_LEGENDRE_H

#include "def.h"

namespace mafox
{
    struct DefaultLegendreCache
    {
        // Is power in cache?
        template <typename IntT>
        bool is_in_cache(IntT power, ENABLE_FN_IF(std::is_integral_v<IntT>)) const noexcept;

        // Let power is n
        // c_inverse_n_plus_1 is 1/(n+1)
        // c_2n_plus_1 is 2n+1
        // c_ means coefficient_
        template <typename IntT>
        void store(IntT power, double c_inverse_n_plus_1, double c_2n_plus_1, ENABLE_FN_IF(std::is_integral_v<IntT>));

        // Returns 1/(power+1)
        template <typename IntT>
        double c_inverse_n_plus_1(IntT power, ENABLE_FN_IF(std::is_integral_v<IntT>)) const; 

        // Returns 2*power+1
        template <typename IntT>
        double c_2n_plus_1(IntT power, ENABLE_FN_IF(std::is_integral_v<IntT>)) const;
    };

    template <typename IntT, typename T>
    auto legendre_polynomial
    (
        IntT power, 
        metaxxa::MinimalArgument<T> x
    );

    // LP means Legendre Polynomial
    // In LaTeX:
    //      $$ CurrentLP is P_n(x)      $$
    //      $$ PreviousLP is P_{n-1}(x) $$
    template <typename IntT, typename T, typename CurrentLP, typename PreviousLP>
    mafox_inline auto legendre_polynomial_next
    (
        IntT power,
        metaxxa::MinimalArgument<T> x,
        metaxxa::MinimalArgument<CurrentLP> current_lp,
        metaxxa::MinimalArgument<PreviousLP> previous_lp
    );

    // LP means Legendre Polynomial
    // In LaTeX:
    //      $$ CurrentLP is P_n(x)      $$
    //      $$ PreviousLP is P_{n-1}(x) $$
    template <typename IntT, typename T, typename CurrentLP, typename PreviousLP>
    auto legendre_polynomial_derivative
    (
        IntT power,
        metaxxa::MinimalArgument<T> x,
        metaxxa::MinimalArgument<CurrentLP> current_lp,
        metaxxa::MinimalArgument<PreviousLP> previous_lp
    );
}

#endif // MAFOX_LEGENDRE_H