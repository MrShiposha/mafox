#ifndef MAFOX_LEGENDRE_INC
#define MAFOX_LEGENDRE_INC

#include "legendre.h"

#include <vector>
#include <utility>

#define _USE_MATH_DEFINES 
#include <math.h>
#include <cmath>
#include <functional>

#ifdef _MSC_VER
#   pragma warning(push)
#   pragma warning(disable: 4003)
#endif // _MSC_VER

#define ENABLE_IF_INT_POWER ENABLE_FN_IF_T(std::is_integral_v<IntT>)

#define MAFOX_SELF LegendrePolynomial<T, IntT>
    
#define MAFOX_LP(ReturnType) \
    template <typename T, typename IntT> \
    ReturnType MAFOX_EXPAND(MAFOX_SELF)

#define INLINE_MAFOX_LP(ReturnType) \
    template <typename T, typename IntT> \
    MAFOX_EXPAND(mafox_inline) ReturnType MAFOX_EXPAND(MAFOX_SELF)

namespace mafox
{
    template <typename T, typename IntT>
    mafox_inline auto legendre_polynomial
    (
        metaxxa::TypeOrRef<const T> x,
        IntT power, 
        ENABLE_IF_INT_POWER
    )
    {
        return legendre_polynomial_pair<T, IntT>(x, power).first;
    }

    template <typename T, typename IntT>
    auto legendre_polynomial_pair
    (
        metaxxa::TypeOrRef<const T> x,
        IntT power,
        ENABLE_IF_INT_POWER
    )
    {
        assert(power >= 0);
        assert(-1.0 < x && x < 1.0);

        if(power == 0)
            return std::pair<T, T>(T(1.0), T(std::nan("Result of Legendre polynomial of power -1")));
        else if(power == 1)
            return std::pair<T, T>(x, T(1.0));

        std::pair<T, T> pair(x /*= P_1(x)*/, T(1.0) /*= P_0(x)*/);

        // i <= power: for pl_n1 == P_power at end of cycle
        for(IntT i = 1; i < power; ++i)
        {
            std::swap(pair.first, pair.second);
            pair.first = legendre_polynomial_next<T, IntT, T, T>(x, i, pair.second, pair.first);
        }

        return pair;
    }

    template <typename T, typename IntT, typename CurrentLP = T, typename PreviousLP = T>
    mafox_inline auto legendre_polynomial_next
    (
        metaxxa::TypeOrRef<const T> x,
        IntT power,
        metaxxa::TypeOrRef<const CurrentLP> cur_lp,
        metaxxa::TypeOrRef<const PreviousLP> prev_lp,
        ENABLE_IF_INT_POWER
    )
    {
        assert(power >= 0);

        return ((2*power + 1) * x * cur_lp - power * prev_lp) / (power + 1);
    }

    template <typename T, typename IntT, typename CurrentLP = T, typename PreviousLP = T>
    mafox_inline auto legendre_polynomial_derivative
    (
        metaxxa::TypeOrRef<const T> x,
        IntT power,
        metaxxa::TypeOrRef<const CurrentLP> cur_lp,
        metaxxa::TypeOrRef<const PreviousLP> prev_lp,
        ENABLE_IF_INT_POWER
    )
    {
        assert(power >= 0);

        if(power == 0)
            return T(0);

        return power*(prev_lp - x * cur_lp) / (1 - x*x);
    }

    template <typename T, typename IntT, typename RootsContainer, typename Eps>
    void legendre_polynomial_roots
    (
        IntT power,
        RootsContainer &roots,
        metaxxa::TypeOrRef<const Eps> eps
    )
    {
        if(power == 0)
            return;

        roots.resize(power);

        if(power == 1)
        {
            roots[0] = T(0.0);
            return;
        }

        IntT root_i = 0;
        auto div = std::div(power, 2);
        IntT end_i = static_cast<IntT>(div.quot);
        
        auto ceil_q = div.quot;
        IntT neg_root_offset = 1;

        if(div.rem != 0)
        {
            roots[div.quot] = T(0.0);
            
            ++ceil_q;
            --neg_root_offset;
            ++root_i;
            ++end_i;
        }

        T guess(0.0), root(0.0);
        T theta(0.0);
        T p_n(0.0), p_n1(0.0), dp_n(0.0);

        IntT power3 = 8*power*power*power;
        auto k1 = 1.0 - (power - 1.0)/power3;
        auto k2 = 1.0/(48*power3*power);
        auto theta_dem = 4*power + 2;

        for(; root_i < end_i; ++root_i)
        {
            theta = M_PI*(4*(ceil_q - root_i) - 1)/theta_dem;
            auto sine = sin(theta);
            sine *= sine;

            guess = (k1 - k2*(39.0 - 28.0/sine))*cos(theta);
            std::tie(p_n, p_n1) = legendre_polynomial_pair<T, IntT>(guess, power);
            dp_n = legendre_polynomial_derivative<T, IntT>(guess, power, p_n, p_n1);

            root = guess - p_n / dp_n;
            while(std::abs(root - guess) >= eps)
            {
                std::tie(p_n, p_n1) = legendre_polynomial_pair<T, IntT>(root, power);
                dp_n = legendre_polynomial_derivative<T, IntT>(root, power, p_n, p_n1);

                guess = root;
                root = guess - p_n / dp_n;
            }

            roots[div.quot + root_i] = root;
            roots[div.quot - root_i - neg_root_offset] = -root;
        }
    }

    template 
    <
        typename T, 
        typename IntT, 
        typename RootsContainer,
        typename Eps
    >
    RootsContainer legendre_polynomial_roots
    (
        IntT power,
        metaxxa::TypeOrRef<const Eps> eps
    )
    {
        RootsContainer container;

        legendre_polynomial_roots<T, IntT, RootsContainer, Eps>(power, container, eps);

        return container;
    }


    INLINE_MAFOX_LP()::LegendrePolynomial()
    : LegendrePolynomial(0)
    {}

    INLINE_MAFOX_LP()::LegendrePolynomial(IntT power)
    : _power(power)
    {}

    INLINE_MAFOX_LP(MAFOX_SELF &)::power(IntT p)
    {
        _power = p;
        return *this;
    }

    INLINE_MAFOX_LP(IntT)::power() const
    {
        return _power;
    }

    INLINE_MAFOX_LP(MAFOX_SELF &)::next_power()
    {
        return power(_power + 1);
    }

    INLINE_MAFOX_LP(auto)::operator()(metaxxa::TypeOrRef<const T> x) const
    {
        return legendre_polynomial<T, IntT>(x, _power);
    }

    INLINE_MAFOX_LP(auto)::pair(metaxxa::TypeOrRef<const T> x) const
    {
        return legendre_polynomial_pair<T, IntT>(x, _power);
    }

    INLINE_MAFOX_LP(auto)::derivative(metaxxa::TypeOrRef<const T> x) const
    {
        if(_power == 0)
            return T(0);

        auto [p_n, p_n1] = pair(x);

        return legendre_polynomial_derivative<T, IntT, decltype(p_n), decltype(p_n1)>
        (
            x,
            _power,
            p_n,
            p_n1
        );
    }

    template <typename T, typename IntT>
    template <typename RootsContainer>
    mafox_inline void MAFOX_SELF::roots(RootsContainer &roots, metaxxa::TypeOrRef<const T> eps)
    {
        return legendre_polynomial_roots<T, IntT, RootsContainer>(_power, roots, eps);
    }

    template <typename T, typename IntT>
    template <typename RootsContainer>
    mafox_inline RootsContainer MAFOX_SELF::roots(metaxxa::TypeOrRef<const T> eps)
    {
        return legendre_polynomial_roots<T, IntT, RootsContainer>(_power, eps);
    }
}

#ifdef _MSC_VER
#   pragma warning(pop)
#endif // _MSC_VER

#undef INLINE_MAFOX_LP
#undef MAFOX_LP
#undef MAFOX_SELF

#undef ENABLE_IF_INT_POWER

#endif // MAFOX_LEGENDRE_INC