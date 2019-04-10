#ifndef MAFOX_LEGENDRE_H
#define MAFOX_LEGENDRE_H

#include <memory>
#include <vector>
#include <future>

#include "def.h"

#define ENABLE_IF_INT_POWER ENABLE_FN_IF(std::is_integral_v<IntT>)

namespace mafox
{
    template <typename T, typename IntT>
    mafox_inline auto legendre_polynomial
    (
        metaxxa::TypeOrRef<const T> x,
        IntT power, 
        ENABLE_IF_INT_POWER
    );

    // Returns std::pair of P_n(x) P_{n-1}(x)
    template <typename T, typename IntT>
    auto legendre_polynomial_pair
    (
        metaxxa::TypeOrRef<const T> x,
        IntT power, 
        ENABLE_IF_INT_POWER
    );

    // LP means Legendre Polynomial
    // In LaTeX:
    //      $$ CurrentLP is P_n(x)      $$
    //      $$ PreviousLP is P_{n-1}(x) $$
    template 
    <
        typename T, 
        typename IntT, 
        typename CurrentLP, 
        typename PreviousLP
    >
    mafox_inline auto legendre_polynomial_next
    (
        metaxxa::TypeOrRef<const T> x,
        IntT power,
        metaxxa::TypeOrRef<const CurrentLP>,
        metaxxa::TypeOrRef<const PreviousLP>,
        ENABLE_IF_INT_POWER
    );

    // LP means Legendre Polynomial
    // In LaTeX:
    //      $$ CurrentLP is P_n(x)      $$
    //      $$ PreviousLP is P_{n-1}(x) $$
    template <typename T, typename IntT, typename CurrentLP, typename PreviousLP>
    mafox_inline auto legendre_polynomial_derivative
    (
        metaxxa::TypeOrRef<const T> x,
        IntT power,
        metaxxa::TypeOrRef<const CurrentLP>,
        metaxxa::TypeOrRef<const PreviousLP>,
        ENABLE_IF_INT_POWER
    );

    template 
    <
        typename T, 
        typename IntT, 
        typename RootsContainer,
        typename Eps = T
    >
    void legendre_polynomial_roots
    (
        IntT power,
        RootsContainer &,
        metaxxa::TypeOrRef<const Eps> eps = MAFOX_DEFAULT_EPS
    );

    template 
    <
        typename T, 
        typename IntT, 
        typename RootsContainer,
        typename Eps = T
    >
    RootsContainer legendre_polynomial_roots
    (
        IntT power,
        metaxxa::TypeOrRef<const Eps> eps = MAFOX_DEFAULT_EPS
    );

    template 
    <
        typename T = double, 
        typename IntT = int
    >
    class LegendrePolynomial
    {
    public:
        mafox_inline LegendrePolynomial();

        mafox_inline LegendrePolynomial(IntT power);

        mafox_inline LegendrePolynomial &power(IntT p);

        mafox_inline IntT power() const;

        mafox_inline LegendrePolynomial &next_power();

        mafox_inline auto operator()(metaxxa::TypeOrRef<const T> x) const;

        mafox_inline auto pair(metaxxa::TypeOrRef<const T> x) const;

        mafox_inline auto derivative(metaxxa::TypeOrRef<const T> x) const;

        template <typename RootsContainer = std::vector<T>>
        mafox_inline void roots(RootsContainer &roots, metaxxa::TypeOrRef<const T> eps = MAFOX_DEFAULT_EPS);

        template <typename RootsContainer = std::vector<T>>
        mafox_inline RootsContainer roots(metaxxa::TypeOrRef<const T> eps = MAFOX_DEFAULT_EPS);

    private:
        IntT _power;
    };
}

#undef ENABLE_IF_INT_POWER

#endif // MAFOX_LEGENDRE_H