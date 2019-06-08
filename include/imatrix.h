#ifndef MAFOX_IMATRIX_H
#define MAFOX_IMATRIX_H

#include <memory>

namespace mafox
{
    struct MatrixTag 
    {
        virtual ~MatrixTag() = default;
    };

    template <typename T>
    class IMatrix : public MatrixTag
    {
    public:
        using difference_type     = std::ptrdiff_t;
        using value_type          = std::remove_cv_t<T>;
        using pointer             = T *;
        using const_pointer       = const T *;
        using reference           = T &;
        using const_reference     = const T &;

        IMatrix() = default;

        IMatrix(const IMatrix &) = default;

        IMatrix(IMatrix &&) = default;

        virtual ~IMatrix() = default;

        IMatrix &operator=(const IMatrix &) = delete;

        IMatrix &operator=(IMatrix &&) = delete;

        virtual std::size_t rows() const = 0;

        virtual std::size_t cols() const = 0;

        virtual reference element(std::size_t i, std::size_t j) = 0;

        virtual const_reference element(std::size_t i, std::size_t j) const = 0;

        virtual void set_element(std::size_t i, std::size_t j, const_reference) = 0;

        virtual void transpose() = 0;

        virtual void transpose_rsd() = 0;

        virtual std::shared_ptr<IMatrix> share_interface() = 0;

        virtual std::shared_ptr<const IMatrix> share_interface() const = 0;
    };
}

#endif // MAFOX_IMATRIX_H