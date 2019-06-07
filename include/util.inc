#ifndef MAFOX_UTIL_INC
#define MAFOX_UTIL_INC

#include "util.h"

namespace mafox
{
    mafox_inline void zero_array(void *s, size_t n)
    {
        volatile char *p = static_cast<volatile char *>(s);
        while (n--) *p++ = 0;
    }
}

#endif // MAFOX_UTIL_INC