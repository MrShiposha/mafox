#ifndef MAFOX_AVECTOR_INC
#define MAFOX_AVECTOR_INC

#include "avector.h"

namespace mafox
{
    template <typename T, typename MatrixHierarchyEnd>
    std::size_t AVector<T, MatrixHierarchyEnd>::rows() const
    {
        MAFOX_FATAL("rows() is meaningless in vector");
    }
    
    template <typename T, typename MatrixHierarchyEnd>
    std::size_t AVector<T, MatrixHierarchyEnd>::cols() const
    {
        MAFOX_FATAL("cols() is meaningless in vector");
    }
    
    template <typename T, typename MatrixHierarchyEnd>
    typename AVector<T, MatrixHierarchyEnd>::reference AVector<T, MatrixHierarchyEnd>::element(std::size_t i, std::size_t j)
    {
        MAFOX_FATAL("element(row, col) is meaningless in vector");
    }
    
    template <typename T, typename MatrixHierarchyEnd>
    typename AVector<T, MatrixHierarchyEnd>::const_reference AVector<T, MatrixHierarchyEnd>::element(std::size_t i, std::size_t j) const
    {
        MAFOX_FATAL("element(row, col) is meaningless in vector");
    }
    
    template <typename T, typename MatrixHierarchyEnd>
    void AVector<T, MatrixHierarchyEnd>::set_element(std::size_t i, std::size_t j, const_reference)
    {
        MAFOX_FATAL("set_element(row, col, value) is meaningless in vector");
    }
    
    template <typename T, typename MatrixHierarchyEnd>
    void AVector<T, MatrixHierarchyEnd>::transpose()
    {
        MAFOX_FATAL("transpose() is meaningless in vector");
    }
    
    template <typename T, typename MatrixHierarchyEnd>
    void AVector<T, MatrixHierarchyEnd>::transpose_rsd()
    {
        MAFOX_FATAL("transpose_rsd() is meaningless in vector");
    }
    
    template <typename T, typename MatrixHierarchyEnd>
    typename AVector<T, MatrixHierarchyEnd>::template matrix_t<T> AVector<T, MatrixHierarchyEnd>::transposed()
    {
        MAFOX_FATAL("transposed() is meaningless in vector");
    }
    
    template <typename T, typename MatrixHierarchyEnd>
    typename AVector<T, MatrixHierarchyEnd>::template matrix_t<T> AVector<T, MatrixHierarchyEnd>::transposed_rsd()
    {
        MAFOX_FATAL("transposed_rsd() is meaningless in vector");
    }

    template <typename T, typename MatrixHierarchyEnd>
    const auto &AVector<T, MatrixHierarchyEnd>::operator()(std::size_t coordinate) const
    {
        return element(coordinate);
    }

    template <typename T, typename MatrixHierarchyEnd>
    bool AVector<T, MatrixHierarchyEnd>::try_set_element(std::size_t coordinate, const_reference value)
    {
        set_element(coordinate, value);
        return true;
    }
}

#endif // MAFOX_AVECTOR_INC