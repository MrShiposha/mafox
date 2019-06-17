#ifndef MAFOX_VECTOR_H
#define MAFOX_VECTOR_H

#include "avector.h"

namespace mafox
{
    template <typename T, typename MatrixHierarchyEnd = This>
    class Vector;

    template <typename T>
    class vector_data_t;

    template <typename T, typename MatrixHierarchyEnd>
    struct MatrixTraits<Vector<T, MatrixHierarchyEnd>>
    {
        MAFOX_DEFAULT_VECTOR_TRAITS(Vector, T, vector_data_t<T>);
    };

    template <typename T, typename MatrixHierarchyEnd>
    class Vector : public MatrixExtender<AVector, Vector<T>, MatrixHierarchyEnd>
    {
    public:
        USING_MAFOX_VECTOR_TYPES(Vector);

        explicit Vector(std::size_t dimension);

        Vector(std::size_t dimension, const T &initial_value);

        Vector(std::initializer_list<T>);

        Vector(const Vector &);

        Vector(Vector &&);

        Vector() = delete;

        virtual ~Vector() = default;

        Vector &operator=(const Vector &);

        Vector &operator=(Vector &&);

        virtual std::size_t dimension() const override;

        virtual reference element(std::size_t coordinate) override;

        virtual const_reference element(std::size_t coordinate) const override;

        virtual void set_element(std::size_t coordinate, const_reference value) override;

        virtual std::shared_ptr<IMatrix<T>> share_interface() override;

        virtual std::shared_ptr<const IMatrix<T>> share_interface() const override;

        virtual shared_data_t shared_data() override;

        virtual const_shared_data_t shared_cdata() const override;

        virtual matrix_t<T> share() override;

    protected:
        Vector(shared_data_t);

        shared_data_t m_data;
    };
}

#endif // MAFOX_VECTOR_H