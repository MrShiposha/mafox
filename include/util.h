#ifndef MAFOX_UTIL_H
#define MAFOX_UTIL_H

#include "def.h"

namespace mafox
{
    template <typename T>
    inline const static T ZERO(0);

    mafox_inline void zero_array(void *s, size_t n);
}

#endif // MAFOX_UTIL_H