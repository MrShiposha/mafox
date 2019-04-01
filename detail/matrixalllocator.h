#ifndef MAFOX_DETAIL_MATRIXALLOCATOR_H
#define MAFOX_DETAIL_MATRIXALLOCATOR_H

#include <memory>

namespace mafox::detail
{
    template <typename T>
    struct MatrixAllocator
    {
        using Type = T;
        using Traits = std::allocator_traits<MatrixAllocator<T>>;

        using value_type      = Type;
        using size_type       = typename std::allocator<T>::size_type;
        using difference_type = typename std::allocator<T>::difference_type;
        using pointer         = typename std::allocator<T>::pointer;
        using const_pointer   = typename std::allocator<T>::const_pointer;
        using reference       = typename std::allocator<T>::reference;
        using const_reference = typename std::allocator<T>::const_reference;

        template<class U> struct rebind 
        {
            using other = ArenaAllocator<U>; 
        };

        template<class U, class... Args> 
        void construct(U* p, Args&&... args);
        
        template<class U> void destroy(U* p);

        MatrixAllocator() = default;

        template <typename U>
        MatrixAllocator(const MatrixAllocator<U> &) = default;

        T *allocate(std::size_t);

        T *reallocate(T *, std::size_t);

        void deallocate(T *, std::size_t);
    };
}

template <typename T, typename U>
bool operator==(const mafox::detail::MatrixAllocator<T> &, const mafox::detail::MatrixAllocator<U> &);

template <typename T, typename U>
bool operator!=(const mafox::detail::MatrixAllocator<T> &, const mafox::detail::MatrixAllocator<U> &);

#endif // MAFOX_DETAIL_MATRIXALLOCATOR_H