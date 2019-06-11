#ifndef MAFOX_MATRIX_H
#define MAFOX_MATRIX_H

#include <memory>

#include "amatrix.h"
#include "matrixorder.h"

namespace mafox
{
    template <typename T>
    class matrix_data_t;

    template <typename T, typename MatrixHierarchyEnd = This>
    class Matrix;

    template <typename T, typename MatrixHierarchyEnd>
    struct MatrixTraits<Matrix<T, MatrixHierarchyEnd>>
    {
        MAFOX_DEFAULT_MATRIX_TRAITS(Matrix, T, matrix_data_t<T>);
    };

    template <typename T, typename MatrixHierarchyEnd>
    class Matrix : public MatrixExtender<AMatrix, Matrix<T>, MatrixHierarchyEnd>
    {
    public:
        USING_MAFOX_MATRIX_TYPES(Matrix);

        Matrix(std::size_t rows, std::size_t cols, MatrixOrder = MatrixOrder::ROW_MAJOR);

        // template <typename Iterator>
        // Matrix(Iterator begin, Iterator end);

        // Matrix(std::initializer_list<std::initializer_list<T>>);

        Matrix(const Matrix &);

        Matrix(Matrix &&);

        virtual ~Matrix();

        Matrix &operator=(const Matrix &);

        Matrix &operator=(Matrix &&);

        mafox_inline virtual std::size_t rows() const override;

        mafox_inline virtual std::size_t cols() const override;

        virtual reference element(std::size_t i, std::size_t j) override;

        virtual const_reference element(std::size_t i, std::size_t j) const override;

        virtual void set_element(std::size_t i, std::size_t j, const_reference) override;

        virtual void transpose() override;

        virtual matrix_t<T> transposed() override;

        virtual void transpose_rsd() override;

        virtual matrix_t<T> transposed_rsd() override;

        virtual shared_data_t shared_data() override;

        virtual const_shared_data_t shared_cdata() const override;

        virtual matrix_t<T> share() override;

        virtual std::shared_ptr<IMatrix<T>> share_interface() override;

        virtual std::shared_ptr<const IMatrix<T>> share_interface() const override;

        pointer data();

        const_pointer cdata() const;

        MatrixOrder order() const;

        void set_order(MatrixOrder);

    private:
        Matrix(shared_data_t);

        shared_data_t m_data;
    };
}

#endif // MAFOX_MATRIX_H