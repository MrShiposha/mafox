#ifndef MAFOX_BANDMATRIX_H
#define MAFOX_BANDMATRIX_H

#include <memory>

#include "amatrix.h"

namespace mafox
{
    template <typename T>
    class band_matrix_data_t;

    template <typename T, typename MatrixHierarchyEnd = This>
    class BandMatrix;

    template <typename T, typename MatrixHierarchyEnd>
    struct MatrixTraits<BandMatrix<T, MatrixHierarchyEnd>>
    {
        MAFOX_DEFAULT_MATRIX_TRAITS(BandMatrix, T, band_matrix_data_t<T>);
    };

    template <typename T, typename MatrixHierarchyEnd>
    class BandMatrix : public MatrixExtender<AMatrix, BandMatrix<T>, MatrixHierarchyEnd>
    {
    public:
        USING_MAFOX_MATRIX_TYPES(BandMatrix);

        BandMatrix(std::size_t size, std::size_t lower_bandwidth, std::size_t upper_bandwidth);

        BandMatrix(const BandMatrix &);

        BandMatrix(BandMatrix &&);

        virtual ~BandMatrix();

        BandMatrix &operator=(const BandMatrix &);

        BandMatrix &operator=(BandMatrix &&);

        // template <typename Iterator>
        // BandMatrix(Iterator begin, Iterator end);

        // BandMatrix(std::initializer_list<std::initializer_list<T>>);

        virtual std::size_t rows() const override;

        virtual std::size_t cols() const override;

        virtual const_reference element(std::size_t i, std::size_t j) const override;

        virtual void set_element(std::size_t i, std::size_t j, const_reference) override;
        
        virtual bool try_set_element(std::size_t i, std::size_t j, const_reference) override;

        virtual void transpose() override;

        virtual matrix_t<T> transposed() override;

        virtual void transpose_rsd() override;

        virtual matrix_t<T> transposed_rsd() override;

        virtual shared_data_t shared_data() override;

        virtual const_shared_data_t shared_cdata() const override;

        virtual matrix_t<T> share() override;

        virtual std::shared_ptr<IMatrix<T>> share_interface() override;

        virtual std::shared_ptr<const IMatrix<T>> share_interface() const override;

        std::size_t lower_bandwidth() const;

        std::size_t upper_bandwidth() const;

        pointer diagonal_data();

        const_pointer diagonal_cdata() const;

        pointer lower_diagonal_data(std::size_t level);

        const_pointer lower_diagonal_cdata(std::size_t level) const;

        pointer upper_diagonal_data(std::size_t level);

        const_pointer upper_diagonal_cdata(std::size_t level) const;

    protected:
        virtual reference element(std::size_t i, std::size_t j) override;

        BandMatrix(shared_data_t);

        shared_data_t m_data;
    };

    template <typename Iterator>
    BandMatrix(Iterator begin, Iterator end) -> BandMatrix<typename Iterator::value_type>;
}

#endif // MAFOX_BANDMATRIX_H