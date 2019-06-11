#ifndef MAFOX_DETAIL_DEF_H
#define MAFOX_DETAIL_DEF_H

#include <type_traits>
#include <stdexcept>

#include "../lib/metaxxa/metaxxa.hpp"

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

#define MAFOX_DEFAULT_EPS 0.001

namespace mafox
{
    class FatalError : public std::runtime_error
    {
    public:
        using std::runtime_error::runtime_error;
    };

    struct This {};
}

#define MAFOX_FATAL(message) do { assert(false && message); throw mafox::FatalError(message); } while(0)

#endif // MAFOX_DETAIL_DEF_H