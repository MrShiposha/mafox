#ifndef MAFOX_LEGENDREСACHEMANAGER_H
#define MAFOX_LEGENDREСACHEMANAGER_H

namespace mafox
{
    struct DefaultLegendreCache;

    // Legendre Сoefficient Cache Manager
    // You can declare you own specialization for type T
    // (e.g. using Type = MyLCacheClass)
    // 
    // Let 
    //      BoolT = any type, that can be casted to bool
    //      IntT = see legendre.h
    //      
    //      n = power of legendre polynomial
    //      alpha = (2*n - 1)/n
    //      beta  = (n-1)/n
    // MyLCacheClass must have following members:
    //      * BoolT is_in_cache(IntT power) const // is coefficients for power in cache?
    //
    //      * void store(IntT power, double alpha, double beta) // store power, alpha and beta in cache 
    //
    //      * double alpha(IntT power) const
    //
    //      * double beta(IntT power) const
    template <typename T>
    struct LegendreCacheManager
    {
        using Type = DefaultLegendreCache;
    };
}

#endif // MAFOX_LEGENDREСACHEMANAGER_H