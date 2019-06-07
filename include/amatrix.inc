#ifndef MAFOX_AMATRIX_INC
#define MAFOX_AMATRIX_INC

#include "amatrix.h"

namespace mafox
{
    template <typename Matrix>
    typename AMatrix<Matrix>::Size AMatrix<Matrix>::size() const
    {
        return Size { rows(), cols() };
    }

    template <typename Matrix>
    bool AMatrix<Matrix>::is_square() const
    {
        return rows() == cols();
    }

    template <typename Matrix>
    mafox_inline typename AMatrix<Matrix>::const_reference 
    AMatrix<Matrix>::operator()(std::size_t i, std::size_t j) const
    {
        return element(i, j);
    }

    template <typename Matrix>
    bool AMatrix<Matrix>::try_set_element(std::size_t i, std::size_t j, const_reference value)
    {
        set_element(i, j, value);
        return true;
    }
}

template <typename T>
std::ostream &operator<<(std::ostream &os, const mafox::AMatrix<T> &matrix)
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