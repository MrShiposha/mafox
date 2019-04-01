#ifndef MAFOX_DETAIL_DEF_H
#define MAFOX_DETAIL_DEF_H

#include <type_traits>

#include "../lib/metaxxa.hpp"

#ifdef _MSC_VER
    #define mafox_inline __mafox_inline
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

#define ENABLE_FN_IF(CONDITION) std::enable_if_t<CONDITION> * = nullptr

namespace mafox::detail
{
    using Byte = unsigned char;
}

#endif // MAFOX_DETAIL_DEF_H