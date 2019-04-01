#ifndef MAFOX_LEGENDREСACHEMANAGER_H
#define MAFOX_LEGENDREСACHEMANAGER_H

#include "legendre.h"

namespace mafox
{
    // Legendre Сoefficient Cache Manager
    // You can declare you own specialization for type T
    // (e.g. using Type = MyLCacheClass)
    // 
    // Let 
    //      BoolT = any type, that can be casted to bool
    //      IntT = see legendre.h
    // MyLCacheClass must have following members:
    //      * BoolT is_in_cache(IntT power) const // is coefficients for power in cache?
    //
    //      * void store(IntT power, double c_inverse_n_plus_1, double c_2n_plus_1) // store power, 1/(power+1) and 2*power+1 in cache 
    //
    //      * double c_inverse_n_plus_1(IntT power) const // returns 1/(power+1)
    //
    //      * double c_2n_plus_1(IntT power) const // return 2n+1
    template <typename T>
    struct LegendreCache
    {
        using Type = DefaultLegendreCache;
    };
}

#endif // MAFOX_LEGENDREСACHEMANAGER_H