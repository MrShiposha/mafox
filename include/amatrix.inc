#ifndef MAFOX_AMATRIX_INC
#define MAFOX_AMATRIX_INC

#include "amatrix.h"

namespace mafox
{
    template <typename Matrix, typename MatrixHierarchyEnd>
    typename AMatrix<Matrix, MatrixHierarchyEnd>::Size AMatrix<Matrix, MatrixHierarchyEnd>::size() const
    {
        return Size { this->rows(), this->cols() };
    }

    template <typename Matrix, typename MatrixHierarchyEnd>
    bool AMatrix<Matrix, MatrixHierarchyEnd>::is_square() const
    {
        return this->rows() == this->cols();
    }

    template <typename Matrix, typename MatrixHierarchyEnd>
    mafox_inline typename AMatrix<Matrix, MatrixHierarchyEnd>::const_reference 
    AMatrix<Matrix, MatrixHierarchyEnd>::operator()(std::size_t i, std::size_t j) const
    {
        return this->element(i, j);
    }

    template <typename Matrix, typename MatrixHierarchyEnd>
    bool AMatrix<Matrix, MatrixHierarchyEnd>::try_set_element(std::size_t i, std::size_t j, const_reference value)
    {
        this->set_element(i, j, value);
        return true;
    }
}

template <typename T, typename MatrixHierarchyEnd>
std::ostream &operator<<(std::ostream &os, const mafox::AMatrix<T, MatrixHierarchyEnd> &matrix)
{
    std::streamsize width = os.width();
    std::streamsize precision = os.precision();

    for(std::size_t i = 0, j = 0; i < matrix.rows(); ++i)
    {
        for(j = 0; j < matrix.cols(); ++j)
            os << std::setw(width) << std::setprecision(precision) << matrix.element(i, j) << ' ';
        os << '\n';
    }

    return os << std::flush;
}

#endif // MAFOX_AMATRIX_INC