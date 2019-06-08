#ifndef MAFOX_AMATRIX_H
#define MAFOX_AMATRIX_H

#include <iostream>
#include <iomanip>
#include <utility>

#include "imatrix.h"
#include "size.h"
#include "def.h"

#define USING_MAFOX_MATRIX_TYPES(Matrix)                                                 \
    template <typename ___MAFOX_T>                                                       \
    using matrix_t            = typename AMatrix<Matrix>::template matrix_t<___MAFOX_T>; \
    using data_t              = typename AMatrix<Matrix>::data_t;                        \
    using shared_data_t       = typename AMatrix<Matrix>::shared_data_t;                 \
    using const_shared_data_t = typename AMatrix<Matrix>::const_shared_data_t;           \
    using difference_type_t   = typename AMatrix<Matrix>::difference_type;               \
    using value_type          = typename AMatrix<Matrix>::value_type;                    \
    using pointer             = typename AMatrix<Matrix>::pointer;                       \
    using const_pointer       = typename AMatrix<Matrix>::const_pointer;                 \
    using reference           = typename AMatrix<Matrix>::reference;                     \
    using const_reference     = typename AMatrix<Matrix>::const_reference;               \
    using Size                = typename AMatrix<Matrix>::Size

#define MAFOX_DEFAULT_MATRIX_TRAITS(user_matrix_t, value_t, user_data_t)   \
        template <typename ___MAFOX_T>                                     \
        using matrix_t            = user_matrix_t<___MAFOX_T>;             \
        using data_t              = user_data_t;                           \
        using shared_data_t       = std::shared_ptr<data_t>;               \
        using const_shared_data_t = std::shared_ptr<const data_t>;         \
        using difference_type     = std::ptrdiff_t;                        \
        using value_type          = std::remove_cv_t<value_t>;             \
        using pointer             = value_t *;                             \
        using const_pointer       = const value_t *;                       \
        using reference           = value_t &;                             \
        using const_reference     = const value_t &

#define MAFOX_INHERIT_TRAITS(base_t)                                                \
        template <typename ___MAFOX_T>                                              \
        using matrix_t            = typename base_t::template matrix_t<___MAFOX_T>; \
        using data_t              = typename base_t::data_t;                        \
        using shared_data_t       = typename base_t::shared_data_t;                 \
        using const_shared_data_t = typename base_t::const_shared_data_t;           \
        using difference_type     = typename base_t::difference_type;               \
        using value_type          = typename base_t::value_type;                    \
        using pointer             = typename base_t::pointer;                       \
        using const_pointer       = typename base_t::const_pointer;                 \
        using reference           = typename base_t::reference;                     \
        using const_reference     = typename base_t::const_reference

namespace mafox
{
    template <typename Matrix>
    struct MatrixTraits;

    template <typename Matrix>
    class AMatrix : public IMatrix<typename MatrixTraits<Matrix>::value_type>
    {
    public:
        using Traits              = MatrixTraits<Matrix>;

        template <typename ___MAFOX_T>
        using matrix_t            = typename Traits::template matrix_t<___MAFOX_T>;

        using data_t              = typename Traits::data_t;
        using shared_data_t       = typename Traits::shared_data_t;
        using const_shared_data_t = typename Traits::const_shared_data_t;

        using value_type          = typename Traits::value_type;
        using Interface           = IMatrix<value_type>;
        using difference_type     = typename Interface::difference_type;
        using pointer             = typename Interface::pointer;
        using const_pointer       = typename Interface::const_pointer;
        using reference           = typename Interface::reference;
        using const_reference     = typename Interface::const_reference;
        
        using Size                = Size2D<std::size_t>;

        AMatrix() = default;

        AMatrix(const AMatrix &) = default;

        AMatrix(AMatrix &&) = default;

        virtual ~AMatrix() = default;

        AMatrix &operator=(const AMatrix &) = delete;

        AMatrix &operator=(AMatrix &&) = delete;

        Size size() const;

        bool is_square() const;

        mafox_inline const_reference operator()(std::size_t i, std::size_t j) const;

        virtual Matrix transposed() = 0;

        virtual Matrix transposed_rsd() = 0;

        virtual bool try_set_element(std::size_t i, std::size_t j, const_reference);

        virtual shared_data_t shared_data() = 0;

        virtual const_shared_data_t shared_cdata() const = 0;

        virtual Matrix share() = 0;
    };
}

template <typename T>
std::ostream &operator<<(std::ostream &, const mafox::AMatrix<T> &);

#endif // MAFOX_AMATRIX_H