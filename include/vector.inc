#ifndef MAFOX_VECTOR_INC
#define MAFOX_VECTOR_INC

#include <cstring>

#include "vector.h"

namespace mafox
{
    template <typename T>
    class raw_vector_data_t;

    template <typename T>
    class vector_data_t
    {
    public:
        virtual ~vector_data_t() {}

        virtual void assign(std::shared_ptr<vector_data_t>) = 0;
        virtual std::size_t dimension() const = 0;
        virtual T &element(std::size_t coordinate) = 0;
        virtual const T &element(std::size_t coordinate) const = 0;
        virtual void set_element(std::size_t coordinate, const T &) = 0;
    };

    template <typename T>
    class raw_vector_data_t : public vector_data_t<T>
    {
    public:
        raw_vector_data_t(std::size_t dimension)
        : dim(dimension), data(new T[dimension])
        {}

        virtual ~raw_vector_data_t()
        {}

        virtual void assign(std::shared_ptr<raw_vector_data_t<T>> m_data)
        {
            dim = m_data->dim;
            data.reset(new T[dim]);
            memcpy(data.get(), m_data->data.get(), dim * sizeof(T));
        }

        virtual void assign(std::shared_ptr<vector_data_t<T>> m_data) override
        {
            if(auto raw_ptr = std::dynamic_pointer_cast<raw_vector_data_t<T>>(m_data))
                assign(raw_ptr);
            else
                MAFOX_FATAL("Invalid vector data");
        }
        
        virtual std::size_t dimension() const override
        {
            return dim;
        }
        
        virtual T &element(std::size_t coordinate) override
        {
            return data[coordinate];
        }

        virtual const T &element(std::size_t coordinate) const override
        {
            return const_cast<raw_vector_data_t<T>*>(this)->element(coordinate);
        }

        virtual void set_element(std::size_t coordinate, const T &value) override
        {
            assert(coordinate < dim);
            element(coordinate) = value;
        }

        std::size_t dim;
        std::unique_ptr<T[]> data;
    };

    template <typename T, typename MatrixHierarchyEnd>
    Vector<T, MatrixHierarchyEnd>::Vector(std::size_t dimension)
    : m_data(std::make_shared<raw_vector_data_t<T>>(dimension))
    {
        zero_array(std::static_pointer_cast<raw_vector_data_t<T>>(m_data)->data.get(), dimension * sizeof(T));
    }

    template <typename T, typename MatrixHierarchyEnd>
    Vector<T, MatrixHierarchyEnd>::Vector(const Vector &other)
    : m_data(std::make_shared<raw_vector_data_t<T>>(other.dimension()))
    {
        m_data->assign(other.m_data);
    }

    template <typename T, typename MatrixHierarchyEnd>
    Vector<T, MatrixHierarchyEnd>::Vector(Vector &&other)
    : m_data(std::move(other.m_data))
    {}

    template <typename T, typename MatrixHierarchyEnd>
    Vector<T, MatrixHierarchyEnd>::Vector(shared_data_t m_data)
    : m_data(m_data)
    {}

    template <typename T, typename MatrixHierarchyEnd>
    Vector<T, MatrixHierarchyEnd> &Vector<T, MatrixHierarchyEnd>::operator=(const Vector &rhs)
    {
        if(this != &rhs)
            *this = std::move(Vector<T, MatrixHierarchyEnd>(rhs));
        return *this;
    }

    template <typename T, typename MatrixHierarchyEnd>
    Vector<T, MatrixHierarchyEnd> &Vector<T, MatrixHierarchyEnd>::operator=(Vector &&rhs)
    {
        if(this != &rhs)
            m_data = std::move(rhs.m_data);
        return *this;
    }

    template <typename T, typename MatrixHierarchyEnd>
    std::size_t Vector<T, MatrixHierarchyEnd>::dimension() const
    {
        return m_data->dimension();
    }

    template <typename T, typename MatrixHierarchyEnd>
    typename Vector<T, MatrixHierarchyEnd>::reference Vector<T, MatrixHierarchyEnd>::element(std::size_t coordinate)
    {
        return m_data->element(coordinate);
    }

    template <typename T, typename MatrixHierarchyEnd>
    typename Vector<T, MatrixHierarchyEnd>::const_reference Vector<T, MatrixHierarchyEnd>::element(std::size_t coordinate) const
    {
        return const_cast<Vector<T, MatrixHierarchyEnd>*>(this)->element(coordinate);
    }

    template <typename T, typename MatrixHierarchyEnd>
    void Vector<T, MatrixHierarchyEnd>::set_element(std::size_t coordinate, const_reference value)
    {
        m_data->set_element(coordinate, value);
    }

    template <typename T, typename MatrixHierarchyEnd>
    std::shared_ptr<IMatrix<T>> Vector<T, MatrixHierarchyEnd>::share_interface()
    {
        return std::shared_ptr<Vector<T, MatrixHierarchyEnd>>(new Vector<T, MatrixHierarchyEnd>(m_data));
    }

    template <typename T, typename MatrixHierarchyEnd>
    std::shared_ptr<const IMatrix<T>> Vector<T, MatrixHierarchyEnd>::share_interface() const
    {
        return std::shared_ptr<const Vector<T, MatrixHierarchyEnd>>(new Vector<T, MatrixHierarchyEnd>(m_data));
    }

    template <typename T, typename MatrixHierarchyEnd>
    typename Vector<T, MatrixHierarchyEnd>::shared_data_t Vector<T, MatrixHierarchyEnd>::shared_data()
    {
        return m_data;
    }

    template <typename T, typename MatrixHierarchyEnd>
    typename Vector<T, MatrixHierarchyEnd>::const_shared_data_t Vector<T, MatrixHierarchyEnd>::shared_cdata() const
    {
        return m_data;
    }

    template <typename T, typename MatrixHierarchyEnd>
    typename Vector<T, MatrixHierarchyEnd>::template matrix_t<T> Vector<T, MatrixHierarchyEnd>::share()
    {
        return Vector<T, MatrixHierarchyEnd>(m_data);
    }
}

#endif // MAFOX_VECTOR_INC