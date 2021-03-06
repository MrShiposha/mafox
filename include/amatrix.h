#ifndef MAFOX_AMATRIX_H
#define MAFOX_AMATRIX_H

#include <iostream>
#include <iomanip>
#include <utility>
#include <initializer_list>

#include "imatrix.h"
#include "size.h"
#include "def.h"

#define MAFOX_AMATRIX(MatrixHierarchyEnd) \
    mafox::AMatrix<typename mafox::MatrixTraits<mafox::detail::EvalHierarchyEnd<MatrixHierarchyEnd>>::value_type, mafox::detail::EvalHierarchyEnd<MatrixHierarchyEnd>>

#define USING_MAFOX_MATRIX_TYPES(MatrixHierarchyEnd)                                                       \
    template <typename ___MAFOX_T>                                                                         \
    using matrix_t            = typename MAFOX_AMATRIX(MatrixHierarchyEnd)::template matrix_t<___MAFOX_T>; \
    using data_t              = typename MAFOX_AMATRIX(MatrixHierarchyEnd)::data_t;                        \
    using shared_data_t       = typename MAFOX_AMATRIX(MatrixHierarchyEnd)::shared_data_t;                 \
    using const_shared_data_t = typename MAFOX_AMATRIX(MatrixHierarchyEnd)::const_shared_data_t;           \
    using difference_type_t   = typename MAFOX_AMATRIX(MatrixHierarchyEnd)::difference_type;               \
    using value_type          = typename MAFOX_AMATRIX(MatrixHierarchyEnd)::value_type;                    \
    using pointer             = typename MAFOX_AMATRIX(MatrixHierarchyEnd)::pointer;                       \
    using const_pointer       = typename MAFOX_AMATRIX(MatrixHierarchyEnd)::const_pointer;                 \
    using reference           = typename MAFOX_AMATRIX(MatrixHierarchyEnd)::reference;                     \
    using const_reference     = typename MAFOX_AMATRIX(MatrixHierarchyEnd)::const_reference;               \
    using Size                = typename MAFOX_AMATRIX(MatrixHierarchyEnd)::Size


#define MAFOX_DEFAULT_MATRIX_TRAITS(user_matrix_t, value_t, user_data_t)   \
    template <typename ___MAFOX_T>                                         \
    using matrix_t            = user_matrix_t<___MAFOX_T>;                 \
    using data_t              = user_data_t;                               \
    using shared_data_t       = std::shared_ptr<data_t>;                   \
    using const_shared_data_t = std::shared_ptr<const data_t>;             \
    using difference_type     = std::ptrdiff_t;                            \
    using value_type          = std::remove_cv_t<value_t>;                 \
    using pointer             = value_t *;                                 \
    using const_pointer       = const value_t *;                           \
    using reference           = value_t &;                                 \
    using const_reference     = const value_t &

#define MAFOX_BASEMATRIXTRAITS(this_t, value_t, base_t) mafox::MatrixTraits<base_t<value_t, this_t<value_t>>>

#define MAFOX_INHERIT_MATRIX_TRAITS(this_t, value_t, base_t)                                                          \
    template <typename ___MAFOX_T>                                                                             \
    using matrix_t            = this_t<___MAFOX_T>;                                                            \
    using data_t              = typename MAFOX_BASEMATRIXTRAITS(this_t, value_t, base_t)::data_t;              \
    using shared_data_t       = typename MAFOX_BASEMATRIXTRAITS(this_t, value_t, base_t)::shared_data_t;       \
    using const_shared_data_t = typename MAFOX_BASEMATRIXTRAITS(this_t, value_t, base_t)::const_shared_data_t; \
    using difference_type     = typename MAFOX_BASEMATRIXTRAITS(this_t, value_t, base_t)::difference_type;     \
    using value_type          = typename MAFOX_BASEMATRIXTRAITS(this_t, value_t, base_t)::value_type;          \
    using pointer             = typename MAFOX_BASEMATRIXTRAITS(this_t, value_t, base_t)::pointer;             \
    using const_pointer       = typename MAFOX_BASEMATRIXTRAITS(this_t, value_t, base_t)::const_pointer;       \
    using reference           = typename MAFOX_BASEMATRIXTRAITS(this_t, value_t, base_t)::reference;           \
    using const_reference     = typename MAFOX_BASEMATRIXTRAITS(this_t, value_t, base_t)::const_reference

namespace mafox
{
    template <typename Matrix>
    struct MatrixTraits;
    
    template 
    <
        template 
        <
            typename T, 
            typename MatrixHierarchyEnd
        > typename BaseMatrix,
        typename DerivedMatrix,
        typename MatrixHierarchyEnd
    >
    using MatrixExtender = 
        BaseMatrix
        <
            typename MatrixTraits<DerivedMatrix>::value_type,
            typename metaxxa::If<std::is_same_v<MatrixHierarchyEnd, mafox::This>>
                ::template Then<DerivedMatrix>
                ::template Else<MatrixHierarchyEnd>
                ::Type
        >;

    template <typename T, typename MatrixHierarchyEnd>
    class AMatrix : public IMatrix<typename MatrixTraits<MatrixHierarchyEnd>::value_type>
    {
    public:
        using Traits              = MatrixTraits<MatrixHierarchyEnd>;

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

        virtual matrix_t<T> transposed() = 0;

        virtual matrix_t<T> transposed_rsd() = 0;

        virtual bool try_set_element(std::size_t i, std::size_t j, const_reference);

        virtual shared_data_t shared_data() = 0;

        virtual const_shared_data_t shared_cdata() const = 0;

        virtual matrix_t<T> share() = 0;

        std::shared_ptr<matrix_t<T>> share_ptr();

        std::shared_ptr<const matrix_t<T>> share_ptr() const;
    };
}

template <typename T, typename MatrixHierarchyEnd>
std::ostream &operator<<(std::ostream &, const mafox::AMatrix<T, MatrixHierarchyEnd> &);

namespace mafox::detail
{
    template <typename _T, typename _MatrixHierarchyEnd>
    struct MatrixInfoImpl
    {
        using T = _T;
        using MatrixHierarchyEnd = _MatrixHierarchyEnd;
    };

    template <typename Matrix>
    using MatrixInfo = metaxxa::MoveParameters<MatrixInfoImpl, Matrix>;

    template <typename Matrix>
    struct EvalHierarchyEndImpl
    {
        using Info = MatrixInfo<Matrix>;
        using Type = typename metaxxa::If<std::is_same_v<typename Info::MatrixHierarchyEnd, mafox::This>>
            ::template Then<Matrix>
            ::template Else<typename Info::MatrixHierarchyEnd>
            ::Type;
    };

    template <typename Matrix>
    using EvalHierarchyEnd = typename EvalHierarchyEndImpl<Matrix>::Type;
}

#endif // MAFOX_AMATRIX_H