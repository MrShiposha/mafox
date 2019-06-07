#ifndef MAFOX_SIZE_H
#define MAFOX_SIZE_H

#include <type_traits>

namespace mafox
{
    template <typename T>
    struct Size2D
    {
        T width;
        T height;
    };

    template <typename T>
    struct Size3D
    {
        T width;
        T height;
        T length;
    };
}

#endif // MAFOX_SIZE_H