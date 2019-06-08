#ifndef MAFOX_AVECTOR_H
#define MAFOX_AVECTOR_H

// TODO VectorTraits (typename template vector_t)

#include "amatrix.h"

namespace mafox
{
    struct VectorTag
    {
        virtual ~VectorTag() = default;
    };

    template <typename Vector>
    class AVector : public VectorTag // TODO : public AMatrix<AVector<Vector>>
    {
    public:
        std::size_t dimension() const
        {
            return static_cast<const Vector *>(this)->rows();
        }

        const auto &operator()(std::size_t i) const
        {
            return (*static_cast<const Vector *>(this))(i, 0);
        }

        void set_element(std::size_t i, typename MatrixTraits<Vector>::const_reference value)
        {
            static_cast<Vector *>(this)->set_element(i, 0, value);
        }
    };
}

#endif // MAFOX_AVECTOR_H