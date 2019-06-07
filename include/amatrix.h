#ifndef MAFOX_AMATRIX_H
#define MAFOX_AMATRIX_H

#include <iostream>
#include <iomanip>
#include <utility>

#include "size.h"
#include "def.h"

#define USING_MAFOX_MATRIX_TYPES(Matrix)                                         \
    using data_t              = typename AMatrix<Matrix>::data_t;                \
    using shared_data_t       = typename AMatrix<Matrix>::shared_data_t;         \
    using const_shared_data_t = typename AMatrix<Matrix>::const_shared_data_t;   \
    using difference_type_t   = typename AMatrix<Matrix>::difference_type;       \
    using value_type          = typename AMatrix<Matrix>::value_type;            \
    using pointer             = typename AMatrix<Matrix>::pointer;               \
    using const_pointer       = typename AMatrix<Matrix>::const_pointer;         \
    using reference           = typename AMatrix<Matrix>::reference;             \
    using const_reference     = typename AMatrix<Matrix>::const_reference;       \
    using Size                = typename AMatrix<Matrix>::Size

#define MAFOX_DEFAULT_TRAITS(value_t, user_data_t)                 \
        using data_t              = user_data_t;                   \
        using shared_data_t       = std::shared_ptr<data_t>;       \
        using const_shared_data_t = std::shared_ptr<const data_t>; \
        using difference_type     = std::ptrdiff_t;                \
        using value_type          = std::remove_cv_t<value_t>;     \
        using pointer             = value_t *;                     \
        using const_pointer       = const value_t *;               \
        using reference           = value_t &;                     \
        using const_reference     = const value_t &

namespace mafox
{
    template <typename Matrix>
    struct MatrixTraits;

    template <typename Matrix>
    class AMatrix
    {
    public:
        using Traits              = MatrixTraits<Matrix>;
        using data_t              = typename Traits::data_t;
        using shared_data_t       = typename Traits::shared_data_t;
        using const_shared_data_t = typename Traits::const_shared_data_t;
        using difference_type     = typename Traits::difference_type;
        using value_type          = typename Traits::value_type;
        using pointer             = typename Traits::pointer;
        using const_pointer       = typename Traits::const_pointer;
        using reference           = typename Traits::reference;
        using const_reference     = typename Traits::const_reference;
        
        using Size                = Size2D<std::size_t>;

        AMatrix() = default;

        AMatrix(const AMatrix &) = default;

        AMatrix(AMatrix &&) = default;

        virtual ~AMatrix() = default;

        AMatrix &operator=(const AMatrix &) = delete;

        AMatrix &operator=(AMatrix &&) = delete;

        virtual std::size_t rows() const = 0;

        virtual std::size_t cols() const = 0;

        Size size() const;

        bool is_square() const;

        virtual reference element(std::size_t i, std::size_t j) = 0;

        virtual const_reference element(std::size_t i, std::size_t j) const = 0;

        mafox_inline const_reference operator()(std::size_t i, std::size_t j) const;

        virtual void set_element(std::size_t i, std::size_t j, const_reference) = 0;

        virtual void transpose() = 0;

        virtual Matrix transposed() = 0;

        virtual void transpose_rsd() = 0;

        virtual Matrix transposed_rsd() = 0;

        virtual bool try_set_element(std::size_t i, std::size_t j, const_reference);

        virtual shared_data_t shared_data() = 0;

        virtual const_shared_data_t shared_cdata() const = 0;

    private:
    };
}

template <typename T>
std::ostream &operator<<(std::ostream &, const mafox::AMatrix<T> &);

#endif // MAFOX_AMATRIX_H