#ifndef MAFOX_DETAIL_DEF_H
#define MAFOX_DETAIL_DEF_H

#include <type_traits>

#include "../lib/metaxxa.hpp"

#ifdef _MSC_VER
    #define mafox_inline __forceinline
#elif defined(__GNUC__)
    #define mafox_inline inline __attribute__((__always_inline__))
#elif defined(__CLANG__)
    #if __has_attribute(__always_inline__)
        #define mafox_inline inline __attribute__((__always_inline__))
    #else
        #define mafox_inline inline
    #endif
#else
    #define mafox_inline inline
#endif

#define MAFOX_EXPAND(...) __VA_ARGS__

#define MAFOX_NCPTR(PTR) const_cast<MAFOX_EXPAND(MAFOX_SELF)*>(PTR)

#define MAFOX_DEFAULT_EPS 0.001

namespace mafox
{
    using Byte = unsigned char;
}

#endif // MAFOX_DETAIL_DEF_H