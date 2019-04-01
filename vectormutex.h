#ifndef MAFOX_VECTORMUTEX_H
#define MAFOX_VECTORMUTEX_H

namespace mafox
{
    // This is a default VectorMutex
    // You can provide you own (pass it into the Matrix template parameter)
    // You must define the Type nested alias (e.g. using Type = std::mutex)
    // (if Type is void === no mutex)
    template <typename T>
    struct VectorMutex
    {
        using Type = void;
    };
}

#endif // MAFOX_VECTORMUTEX_H