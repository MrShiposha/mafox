#ifndef MAFOX_SIZE_H
#define MAFOX_SIZE_H

#include <type_traits>

namespace mafox
{
    template <std::size_t N, typename T>
    struct SizeND
    {
        T dimension[N];

        template <std::size_t INDEX>
        auto &get()
        {
            static_assert(INDEX < N);

            return dimension[INDEX];
        }

        template <std::size_t INDEX>
        const auto &get() const
        {
            return const_cast<SizeND*>(this)->template get<INDEX>();
        }
    };

    template <typename T>
    struct SizeND<1, T>
    {
        template <std::size_t INDEX>
        auto &get()
        {
            static_assert(INDEX < 1);

            return dimension[INDEX];
        }

        template <std::size_t INDEX>
        const auto &get() const
        {
            return const_cast<SizeND*>(this)->template get<INDEX>();
        }

        T &length()
        {
            return dimension[0];
        }

        const T &length() const
        {
            return const_cast<SizeND*>(this)->length();
        }

        T dimension[1];
    };

    template <typename T>
    struct SizeND<2, T>
    {
        template <std::size_t INDEX>
        auto &get()
        {
            static_assert(INDEX < 2);

            return dimension[INDEX];
        }

        template <std::size_t INDEX>
        const auto &get() const
        {
            return const_cast<SizeND*>(this)->template get<INDEX>();
        }

        T &width()
        {
            return dimension[0];
        }

        const T &width() const
        {
            return const_cast<SizeND*>(this)->width();
        }

        T &height()
        {
            return dimension[1];
        }

        const T &height() const
        {
            return const_cast<SizeND*>(this)->height();
        }

        T dimension[2];
    };

    template <typename T>
    struct SizeND<3, T>
    {
        template <std::size_t INDEX>
        auto &get()
        {
            static_assert(INDEX < 3);

            return dimension[INDEX];
        }

        template <std::size_t INDEX>
        const auto &get() const
        {
            return const_cast<SizeND*>(this)->template get<INDEX>();
        }

        T &width()
        {
            return dimension[0];
        }

        const T &width() const
        {
            return const_cast<SizeND*>(this)->width();
        }

        T &height()
        {
            return dimension[1];
        }

        const T &height() const
        {
            return const_cast<SizeND*>(this)->height();
        }

        T &length()
        {
            return dimension[2];
        }

        const T &length() const
        {
            return const_cast<SizeND*>(this)->length();
        }

        T dimension[3];
    };

    template <typename T>
    using Size1D = SizeND<1, T>;

    template <typename T>
    using Size2D = SizeND<2, T>;

    template <typename T>
    using Size3D = SizeND<3, T>;
}

namespace std
{
    template <std::size_t INDEX, std::size_t N, typename T>
    class tuple_element<INDEX, mafox::SizeND<N, T>>
    {
    public:
        using type = T;
    };

    template <std::size_t N, typename T>
    class tuple_size<mafox::SizeND<N, T>> : public integral_constant<size_t, N>
    {};

    template <std::size_t INDEX, std::size_t N, typename T>
    auto &get(mafox::SizeND<N, T> &size) noexcept
    {
        return size.template get<INDEX>();
    }

    template <std::size_t INDEX, std::size_t N, typename T>
    const auto &get(const mafox::SizeND<N, T> &size) noexcept
    {
        return std::get<INDEX>(const_cast<mafox::SizeND<N, T> &>(size));
    }
}

#endif // MAFOX_SIZE_H