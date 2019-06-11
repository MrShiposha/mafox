#ifndef MAFOX_AVECTOR_H
#define MAFOX_AVECTOR_H

#include "amatrix.h"

#define USING_MAFOX_VECTOR_TYPES(MatrixHierarchyEnd)                                                       \
    template <typename ___MAFOX_T>                                                                         \
    using matrix_t            = typename MAFOX_AMATRIX(MatrixHierarchyEnd)::template matrix_t<___MAFOX_T>; \
    template <typename ___MAFOX_T>                                                                         \
    using vector_t            = matrix_t<___MAFOX_T>;                                                      \
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

#define MAFOX_DEFAULT_VECTOR_TRAITS(user_matrix_t, value_t, user_data_t)   \
    template <typename ___MAFOX_T>                                         \
    using matrix_t            = user_matrix_t<___MAFOX_T>;                 \
    template <typename ___MAFOX_T>                                         \
    using vector_t            = matrix_t<___MAFOX_T>;                      \
    using data_t              = user_data_t;                               \
    using shared_data_t       = std::shared_ptr<data_t>;                   \
    using const_shared_data_t = std::shared_ptr<const data_t>;             \
    using difference_type     = std::ptrdiff_t;                            \
    using value_type          = std::remove_cv_t<value_t>;                 \
    using pointer             = value_t *;                                 \
    using const_pointer       = const value_t *;                           \
    using reference           = value_t &;                                 \
    using const_reference     = const value_t &

#define MAFOX_INHERIT_VECTOR_TRAITS(this_t, value_t, base_t)                                                   \
    template <typename ___MAFOX_T>                                                                             \
    using matrix_t            = this_t<___MAFOX_T>;                                                            \
    template <typename ___MAFOX_T>                                                                             \
    using vector_t            = matrix_t<___MAFOX_T>;                                                          \
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
    struct VectorTag
    {
        virtual ~VectorTag() = default;
    };

    template <typename T, typename MatrixHierarchyEnd>
    class AVector : 
        public MatrixExtender<AMatrix, MatrixHierarchyEnd, MatrixHierarchyEnd>, 
        public VectorTag
    {
    public:
        USING_MAFOX_VECTOR_TYPES(MatrixHierarchyEnd);
        using base_matrix_t = MatrixExtender<::mafox::AMatrix, MatrixHierarchyEnd, MatrixHierarchyEnd>;
        
    private:
        // These methods are meaningless in vector
        virtual std::size_t rows() const override;
        virtual std::size_t cols() const override;
        virtual reference element(std::size_t i, std::size_t j) override;
        virtual const_reference element(std::size_t i, std::size_t j) const override;
        virtual void set_element(std::size_t i, std::size_t j, const_reference) override;
        virtual void transpose() override;
        virtual void transpose_rsd() override;
        virtual matrix_t<T> transposed() override;
        virtual matrix_t<T> transposed_rsd() override;
        
        using base_matrix_t::size;
        using base_matrix_t::is_square;
        using base_matrix_t::operator();
        using base_matrix_t::try_set_element;

    public:
        AVector() = default;

        AVector(const AVector &) = default;

        AVector(AVector &&) = default;

        virtual ~AVector() = default;

        AVector &operator=(const AVector &) = default;

        AVector &operator=(AVector &&) = default;

        virtual std::size_t dimension() const = 0;

        virtual reference element(std::size_t coordinate) = 0;

        virtual const_reference element(std::size_t coordinate) const = 0;

        virtual void set_element(std::size_t coordinate, const_reference value) = 0;

        const auto &operator()(std::size_t coordinate) const;

        virtual bool try_set_element(std::size_t coordinate, const_reference value);
    };

    template <typename T>
    constexpr bool is_vector() { return std::is_base_of_v<VectorTag, T>; }

    template <typename T>
    constexpr bool is_exactly_matrix() { return is_matrix<T>() && !is_vector<T>(); }
}

template <typename T, typename MatrixHierarchyEnd>
std::ostream &operator<<(std::ostream &, const mafox::AVector<T, MatrixHierarchyEnd> &);

#endif // MAFOX_AVECTOR_H