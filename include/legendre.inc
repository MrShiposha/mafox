#include "legendre.h"

#include <vector>
#include <utility>
#include <cmath>
#include <functional>

#include "rootsolver.inc"

#define ENABLE_IF_INT_POWER ENABLE_FN_IF_T(std::is_integral_v<IntT>)

#define MAFOX_SELF LegendrePolynomial<T, IntT, Cache>
    
#define MAFOX_LP(ReturnType) \
    template <typename T, typename IntT, typename Cache> \
    ReturnType MAFOX_EXPAND(MAFOX_SELF)

#define INLINE_MAFOX_LP(ReturnType) \
    template <typename T, typename IntT, typename Cache> \
    MAFOX_EXPAND(mafox_inline) ReturnType MAFOX_EXPAND(MAFOX_SELF)

namespace mafox
{
    namespace detail
    {
        class DefaultLegendreCache
        {
        public:
            using Alpha = double;
            using Beta  = double;

            std::vector<std::pair<Alpha, Beta>> cache;
        };

        template <typename T>
        static inline typename LegendreCacheManager<T>::Type default_cache;
    }

    mafox_inline DefaultLegendreCache::DefaultLegendreCache()
    : self(new detail::DefaultLegendreCache())
    {
        self->cache.push_back(std::make_pair(std::nan("Legendre alpha for n == 0"), std::nan("Legendre beta for n == 0")));
        self->cache.push_back(std::make_pair(1., 0.));
    }

    mafox_inline DefaultLegendreCache::DefaultLegendreCache(const DefaultLegendreCache &other)
    : self(new detail::DefaultLegendreCache())
    {
        self->cache = other.self->cache;
    }

    mafox_inline DefaultLegendreCache::DefaultLegendreCache(DefaultLegendreCache &&other)
    : self(std::move(other.self))
    {}

    template <typename IntT>
    mafox_inline bool DefaultLegendreCache::is_in_cache(IntT power, ENABLE_IF_INT_POWER) const noexcept
    {
        return self->cache.size() > power;
    }

    template <typename IntT>
    mafox_inline void DefaultLegendreCache::store(IntT power, double alpha, double beta, ENABLE_IF_INT_POWER)
    {
        if(is_in_cache(power))
            self->cache[power] = std::make_pair(alpha, beta);
        else
        {
            assert(power == self->cache.size() && "INTERNAL ERROR: DefaultLegendreCache can't store nonlinear");

            self->cache.push_back(std::make_pair(alpha, beta));
        }
    }

    template <typename IntT>
    mafox_inline double DefaultLegendreCache::alpha(IntT power, ENABLE_IF_INT_POWER) const
    {
        assert(is_in_cache(power));

        return self->cache[power].first;
    }

    template <typename IntT>
    mafox_inline double DefaultLegendreCache::beta(IntT power, ENABLE_IF_INT_POWER) const
    {
        assert(is_in_cache(power));

        return self->cache[power].second;
    }

    template <typename T, typename IntT>
    mafox_inline auto legendre_polynomial
    (
        metaxxa::TypeOrRef<const T> x,
        IntT power, 
        ENABLE_IF_INT_POWER
    )
    {
        return legendre_polynomial<T, IntT>(x, power, detail::default_cache<T>);
    }

    template <typename T, typename IntT, typename Cache>
    mafox_inline auto legendre_polynomial
    (
        metaxxa::TypeOrRef<const T> x,
        IntT power, 
        Cache &cache,
        ENABLE_IF_INT_POWER
    )
    {
        return legendre_polynomial_pair<T, IntT>(x, power, cache).first;
    }

    template <typename T, typename IntT>
    auto legendre_polynomial_pair
    (
        metaxxa::TypeOrRef<const T> x,
        IntT power, 
        ENABLE_IF_INT_POWER
    )
    {
        return legendre_polynomial_pair<T, IntT>(x, power, detail::default_cache<T>);
    }

    template <typename T, typename IntT, typename Cache>
    auto legendre_polynomial_pair
    (
        metaxxa::TypeOrRef<const T> x,
        IntT power,
        Cache &cache,
        ENABLE_IF_INT_POWER
    )
    {
        assert(power >= 0);
        
        if(power == 0)
            return std::pair<T, T>(1.0, std::nan("Result of Legendre polynomial of power -1"));
        else if(power == 1)
            return std::pair<T, T>(x, 1.0);

        // Start from power (n) == 2
        T pl_n1     = x;   // P_{n-1}(x) (now P_1(x))
        T pl_n2     = 1.0; // P_{n-2}(x) (now P_0(x))
        T old_pl_n1 = 0.0; // 0.0 is for init value

        // i <= power: for pl_n1 == P_power at end of cycle
        for(IntT i = 2; i <= power; ++i)
        {
            old_pl_n1 = pl_n1;
            pl_n1 = legendre_polynomial_next<T, IntT, T, T>(x, i, pl_n1, pl_n2, cache);
            pl_n2 = old_pl_n1;
        }

        return std::make_pair(pl_n1, pl_n2);
    }

    template <typename T, typename IntT, typename LP_n1, typename LP_n2, typename Cache>
    mafox_inline auto legendre_polynomial_next
    (
        metaxxa::TypeOrRef<const T> x,
        IntT power,
        metaxxa::TypeOrRef<const LP_n1> lp_n1,
        metaxxa::TypeOrRef<const LP_n2> lp_n2,
        Cache &cache,
        ENABLE_IF_INT_POWER
    )
    {
        assert(power >= 0);

        if(!cache.is_in_cache(power))
            cache.store(power, (2*power - 1)/static_cast<T>(power), (power - 1)/static_cast<T>(power));
        
        return cache.alpha(power) * x * lp_n1 - cache.beta(power)*lp_n2;
    }

    template <typename T, typename IntT, typename LP_n, typename LP_n1>
    mafox_inline auto legendre_polynomial_derivative
    (
        metaxxa::TypeOrRef<const T> x,
        IntT power,
        metaxxa::TypeOrRef<LP_n> lp_n,
        metaxxa::TypeOrRef<const LP_n1> lp_n1,
        ENABLE_IF_INT_POWER
    )
    {
        assert(power >= 0);

        if(power == 0)
            return T(0);

        const T k = power/(1 - x*x);
        return k*(lp_n1 - x * lp_n);
    }

    template <typename T, typename IntT, typename RootsContainer>
    void legendre_polynomial_roots
    (
        IntT power,
        RootsContainer &roots,
        metaxxa::TypeOrRef<const T> eps
    )
    {
        return legendre_polynomial_roots<T, IntT, RootsContainer>(power, roots, detail::default_cache<T>, eps);
    }

    template 
    <
        typename T, 
        typename IntT, 
        typename RootsContainer, 
        typename Cache
    >
    void legendre_polynomial_roots
    (
        IntT power,
        RootsContainer &roots,
        Cache &cache,
        metaxxa::TypeOrRef<const T> eps
    )
    {
        if(power == 0)
            return;

        LegendrePolynomial<T, IntT, Cache> polynomial(power, cache);
        for(IntT i = 1; i <= power; ++i)
        {
            roots.push_back
            (
                RootSolver::newton
                (
                    polynomial, // TODO: LOOK AT CACHE!!!
                    [&polynomial](auto x) { return polynomial.derivative(x); },
                    cos(M_PI*(4*i - 1)/(4*power + 2)),
                    eps
                )
            );
        }
    }

    template <typename T, typename IntT, typename RootsContainer>
    mafox_inline RootsContainer legendre_polynomial_roots(IntT power, metaxxa::TypeOrRef<const T> eps)
    {
        return legendre_polynomial_roots<T, IntT, RootsContainer>(power, detail::default_cache<T>, eps);
    }

    template 
    <
        typename T, 
        typename IntT, 
        typename RootsContainer,
        typename Cache
    >
    mafox_inline RootsContainer legendre_polynomial_roots
    (
        IntT power, 
        Cache &cache,
        metaxxa::TypeOrRef<const T> eps
    )
    {
        RootsContainer roots;

        legendre_polynomial_roots<T, IntT, RootsContainer, Cache>(power, roots, cache, eps);

        return roots;
    }

    INLINE_MAFOX_LP()::LegendrePolynomial()
    : LegendrePolynomial(0)
    {}

    INLINE_MAFOX_LP()::LegendrePolynomial(const Cache &cache)
    : LegendrePolynomial(0, cache)
    {}

    INLINE_MAFOX_LP()::LegendrePolynomial(Cache &&cache)
    : LegendrePolynomial(0, std::move(cache))
    {}

    INLINE_MAFOX_LP()::LegendrePolynomial(IntT power)
    : _power(power), cache()
    {}

    INLINE_MAFOX_LP()::LegendrePolynomial(IntT power, const Cache &cache)
    : _power(power), cache(cache)
    {}

    INLINE_MAFOX_LP()::LegendrePolynomial(IntT power, Cache &&cache)
    : _power(power), cache(std::move(cache))
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
        return legendre_polynomial<T, IntT>(x, _power, cache);
    }

    INLINE_MAFOX_LP(auto)::derivative(metaxxa::TypeOrRef<const T> x) const
    {
        if(_power == 0)
            return T(0);

        auto [p_n, p_n1] = legendre_polynomial_pair<T, IntT>(x, _power, cache);

        return derivative
        (
            x,
            p_n,
            p_n1
        );
    }

    INLINE_MAFOX_LP(auto)::derivative
    (
        metaxxa::TypeOrRef<const T> x,
        metaxxa::TypeOrRef<const T> lp_n,
        metaxxa::TypeOrRef<const T> lp_n1
    ) const
    {
        return derivative<T, T>(x, lp_n, lp_n1);
    }

    template <typename T, typename IntT, typename Cache>
    template <typename LP_n, typename LP_n1>
    mafox_inline auto MAFOX_SELF::derivative
    (
        metaxxa::TypeOrRef<const T> x,
        metaxxa::TypeOrRef<const LP_n> lp_n,
        metaxxa::TypeOrRef<const LP_n1> lp_n1
    ) const
    {
        return legendre_polynomial_derivative<T, IntT, LP_n, LP_n1>(x, _power, lp_n, lp_n1);
    }

    template <typename T, typename IntT, typename Cache>
    template <typename RootsContainer>
    mafox_inline void MAFOX_SELF::roots(RootsContainer &roots, metaxxa::TypeOrRef<const T> eps)
    {
        return legendre_polynomial_roots<T, IntT, RootsContainer, Cache>(_power, roots, cache, eps);
    }

    template <typename T, typename IntT, typename Cache>
    template <typename RootsContainer>
    mafox_inline RootsContainer MAFOX_SELF::roots(metaxxa::TypeOrRef<const T> eps)
    {
        return legendre_polynomial_roots<T, IntT, RootsContainer, Cache>(_power, cache, eps);
    }
}

#undef INLINE_MAFOX_LP
#undef MAFOX_LP
#undef MAFOX_SELF

#undef ENABLE_IF_INT_POWER