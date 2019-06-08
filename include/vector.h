#ifndef MAFOX_VECTOR_H
#define MAFOX_VECTOR_H

// TODO remove
#include "matrix.h"

#include "avector.h"

// TODO

namespace mafox
{
    template <typename T>
    class Vector;

    template <typename T>
    struct MatrixTraits<Vector<T>>
    {
        using const_reference = const T &;
    };

    template <typename T>
    class Vector : public Matrix<T>, public AVector<Vector<T>>
    {
    public:
        using AVector<Vector<T>>::set_element;
        using AVector<Vector<T>>::operator();

        using Matrix<T>::set_element;
        using Matrix<T>::operator();

        template <typename ___MAFOX_T>
        using vector_t = Vector<___MAFOX_T>;

        Vector(std::size_t size)
        : Matrix<T>(size, 1), AVector<Vector<T>>()
        {}
    };
}

#endif // MAFOX_VECTOR_H