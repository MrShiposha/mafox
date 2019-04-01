#ifndef MAFOX_VECTORMANAGER_H
#define MAFOX_VECTORMANAGER_H

#include <cstddef>
#include <cstdint>

template <typename T, std::size_t DIMENSION>
struct VectorManager
{
    // constexpr HAS_ADDING = metaxxa::is_valid<T[]>([](auto &&r, auto &&a, auto &&b) 
    //          -> decltype(vector_add<DIMENSION>(r, a, b)) {});
};

#endif // MAFOX_VECTORMANAGER_H