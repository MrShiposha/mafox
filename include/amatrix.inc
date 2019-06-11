#ifndef MAFOX_AMATRIX_INC
#define MAFOX_AMATRIX_INC

#include "amatrix.h"

namespace mafox
{
    template <typename T, typename MatrixHierarchyEnd>
    typename AMatrix<T, MatrixHierarchyEnd>::Size AMatrix<T, MatrixHierarchyEnd>::size() const
    {
        return Size { this->rows(), this->cols() };
    }

    template <typename T, typename MatrixHierarchyEnd>
    bool AMatrix<T, MatrixHierarchyEnd>::is_square() const
    {
        return this->rows() == this->cols();
    }

    template <typename T, typename MatrixHierarchyEnd>
    mafox_inline typename AMatrix<T, MatrixHierarchyEnd>::const_reference 
    AMatrix<T, MatrixHierarchyEnd>::operator()(std::size_t i, std::size_t j) const
    {
        return this->element(i, j);
    }

    template <typename T, typename MatrixHierarchyEnd>
    bool AMatrix<T, MatrixHierarchyEnd>::try_set_element(std::size_t i, std::size_t j, const_reference value)
    {
        this->set_element(i, j, value);
        return true;
    }

    template <typename T, typename MatrixHierarchyEnd>
    std::shared_ptr<typename AMatrix<T, MatrixHierarchyEnd>::template matrix_t<T>> AMatrix<T, MatrixHierarchyEnd>::share_ptr()
    {
        return std::make_shared<matrix_t<T>>(std::move(share()));
    }

    template <typename T, typename MatrixHierarchyEnd>
    std::shared_ptr<const typename AMatrix<T, MatrixHierarchyEnd>::template matrix_t<T>> AMatrix<T, MatrixHierarchyEnd>::share_ptr() const
    {
        return std::const_pointer_cast<const matrix_t<T>>(const_cast<AMatrix<T, MatrixHierarchyEnd>*>(this)->share_ptr());
    }
}

template <typename T, typename MatrixHierarchyEnd>
std::ostream &operator<<(std::ostream &os, const mafox::AMatrix<T, MatrixHierarchyEnd> &matrix)
{
    std::streamsize width = os.width();
    std::streamsize precision = os.precision();
    char fill = os.fill();

    for(std::size_t i = 0, j = 0; i < matrix.rows(); ++i)
    {
        for(j = 0; j < matrix.cols(); ++j)
            os << std::setw(width) << std::setprecision(precision) << std::setfill(fill) << matrix.element(i, j) << ' ';
        os << '\n';
    }

    return os << std::flush;
}

#endif // MAFOX_AMATRIX_INC