#ifndef MAFOX_LEGENDRE_H
#define MAFOX_LEGENDRE_H

#include <memory>
#include <vector>
#include <future>

#include "def.h"
#include "legendrecachemanager.h"

#define ENABLE_IF_INT_POWER ENABLE_FN_IF(std::is_integral_v<IntT>)

namespace mafox
{
    namespace detail
    {
        class DefaultLegendreCache;
    }

    struct DefaultLegendreCache
    {
        mafox_inline DefaultLegendreCache();

        mafox_inline DefaultLegendreCache(const DefaultLegendreCache &);

        mafox_inline DefaultLegendreCache(DefaultLegendreCache &&);

        // Is power in cache?
        template <typename IntT>
        mafox_inline bool is_in_cache(IntT power, ENABLE_IF_INT_POWER) const noexcept;

        template <typename IntT>
        mafox_inline void store(IntT power, double alpha, double beta, ENABLE_IF_INT_POWER);

        // Returns (2*power - 1)/power
        template <typename IntT>
        mafox_inline double alpha(IntT power, ENABLE_IF_INT_POWER) const; 

        // Returns (power - 1)/power
        template <typename IntT>
        mafox_inline double beta(IntT power, ENABLE_IF_INT_POWER) const;

    private:
        std::unique_ptr<detail::DefaultLegendreCache> self;
    };

    template <typename T, typename IntT>
    mafox_inline auto legendre_polynomial
    (
        metaxxa::TypeOrRef<const T> x,
        IntT power, 
        ENABLE_IF_INT_POWER
    );

    template <typename T, typename IntT, typename Cache = typename LegendreCacheManager<T>::Type>
    mafox_inline auto legendre_polynomial
    (
        metaxxa::TypeOrRef<const T> x,
        IntT power,
        Cache &,
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

    // Returns std::pair of P_n(x) P_{n-1}(x)
    template <typename T, typename IntT, typename Cache = typename LegendreCacheManager<T>::Type>
    auto legendre_polynomial_pair
    (
        metaxxa::TypeOrRef<const T> x,
        IntT power,
        Cache &,
        ENABLE_IF_INT_POWER
    );

    // LP means Legendre Polynomial
    // In LaTeX:
    //      $$ LP_n1 is P_{n-1}(x) $$
    //      $$ LP_n2 is P_{n-2}(x) $$
    template 
    <
        typename T, 
        typename IntT, 
        typename LP_n1, 
        typename LP_n2, 
        typename Cache = typename LegendreCacheManager<T>::Type
    >
    mafox_inline auto legendre_polynomial_next
    (
        metaxxa::TypeOrRef<const T> x,
        IntT power,
        metaxxa::TypeOrRef<const LP_n1> lp_n1,
        metaxxa::TypeOrRef<const LP_n2> lp_n2,
        Cache &,
        ENABLE_IF_INT_POWER
    );

    // LP means Legendre Polynomial
    // In LaTeX:
    //      $$ LP_n is P_n(x) $$
    //      $$ LP_n1 is P_{n-1}(x) $$
    template <typename T, typename IntT, typename LP_n, typename LP_n1>
    mafox_inline auto legendre_polynomial_derivative
    (
        metaxxa::TypeOrRef<const T> x,
        IntT power,
        metaxxa::TypeOrRef<LP_n> lp_n,
        metaxxa::TypeOrRef<const LP_n1> lp_n1,
        ENABLE_IF_INT_POWER
    );

    template 
    <
        typename T, 
        typename IntT, 
        typename RootsContainer
    >
    void legendre_polynomial_roots
    (
        IntT power,
        RootsContainer &,
        metaxxa::TypeOrRef<const T> eps = MAFOX_DEFAULT_EPS
    );

    template
    <
        typename T, 
        typename IntT,
        typename RootsContainer, 
        typename Cache = typename LegendreCacheManager<T>::Type
    >
    void legendre_polynomial_roots
    (
        IntT power,
        RootsContainer &,
        Cache &cache,
        metaxxa::TypeOrRef<const T> eps = MAFOX_DEFAULT_EPS
    );

    template 
    <
        typename T, 
        typename IntT, 
        typename RootsContainer
    >
    mafox_inline RootsContainer legendre_polynomial_roots
    (
        IntT power, 
        metaxxa::TypeOrRef<const T> eps = MAFOX_DEFAULT_EPS
    );

    template 
    <
        typename T, 
        typename IntT, 
        typename RootsContainer,
        typename Cache = typename LegendreCacheManager<T>::Type
    >
    mafox_inline RootsContainer legendre_polynomial_roots
    (
        IntT power,
        Cache &cache,
        metaxxa::TypeOrRef<const T> eps = MAFOX_DEFAULT_EPS
    );

    // TODO: Add async functions

    template 
    <
        typename T = double, 
        typename IntT = int, 
        typename Cache = typename LegendreCacheManager<T>::Type
    >
    class LegendrePolynomial
    {
    public:
        mafox_inline LegendrePolynomial();

        mafox_inline LegendrePolynomial(const Cache &cache);

        mafox_inline LegendrePolynomial(Cache &&cache);

        mafox_inline LegendrePolynomial(IntT power);

        mafox_inline LegendrePolynomial(IntT power, const Cache &cache);

        mafox_inline LegendrePolynomial(IntT power, Cache &&cache);

        mafox_inline LegendrePolynomial &power(IntT p);

        mafox_inline IntT power() const;

        mafox_inline LegendrePolynomial &next_power();

        mafox_inline auto operator()(metaxxa::TypeOrRef<const T> x) const;

        mafox_inline auto derivative(metaxxa::TypeOrRef<const T> x) const;

        mafox_inline auto derivative
        (
            metaxxa::TypeOrRef<const T> x,
            metaxxa::TypeOrRef<const T> lp_n,
            metaxxa::TypeOrRef<const T> lp_n1
        ) const;

        template <typename LP_n, typename LP_n1>
        mafox_inline auto derivative
        (
            metaxxa::TypeOrRef<const T> x,
            metaxxa::TypeOrRef<const LP_n> lp_n,
            metaxxa::TypeOrRef<const LP_n1> lp_n1
        ) const;

        template <typename RootsContainer = std::vector<T>>
        mafox_inline void roots(RootsContainer &roots, metaxxa::TypeOrRef<const T> eps = MAFOX_DEFAULT_EPS);

        template <typename RootsContainer = std::vector<T>>
        mafox_inline RootsContainer roots(metaxxa::TypeOrRef<const T> eps = MAFOX_DEFAULT_EPS);

        // template <typename RootsContainer = std::vector<T>>
        // mafox_inline std::future<RootsContainer &> roots_async(std::launch policy, RootsContainer &roots, metaxxa::TypeOrRef<const T> eps = MAFOX_DEFAULT_EPS);

        template <typename RootsContainer = std::vector<T>>
        mafox_inline std::future<RootsContainer> roots_async(std::launch policy, metaxxa::TypeOrRef<const T> eps = MAFOX_DEFAULT_EPS);

    private:
        IntT _power;
        mutable Cache cache;
    };
}

#undef ENABLE_IF_INT_POWER

#endif // MAFOX_LEGENDRE_H