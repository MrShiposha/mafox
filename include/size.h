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

namespace std
{
    template <std::size_t INDEX, typename T>
    class tuple_element<INDEX, mafox::Size2D<T>>
    {
    public:
        using type = T;
    };

    template <typename T>
    class tuple_size<mafox::Size2D<T>>
    {
    public:
        static constexpr std::size_t value = 2;
    };

    template <std::size_t INDEX, typename T>
    auto get(const mafox::Size2D<T> &size)
    {
        if constexpr(INDEX == 0)
            return size.width;
        else if constexpr(INDEX == 1)
            return size.height;
    }

    template <std::size_t INDEX, typename T>
    class tuple_element<INDEX, mafox::Size3D<T>>
    {
    public:
        using type = T;
    };

    template <typename T>
    class tuple_size<mafox::Size3D<T>>
    {
    public:
        static constexpr std::size_t value = 3;
    };

    template <std::size_t INDEX, typename T>
    auto get(const mafox::Size3D<T> &size)
    {
        if constexpr(INDEX == 0)
            return size.width;
        else if constexpr(INDEX == 1)
            return size.height;
        else if constexpr(INDEX == 1)
            return size.length;
    }
}

#endif // MAFOX_SIZE_H