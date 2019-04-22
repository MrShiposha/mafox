#ifndef MAFOX_TABLE_INC
#define MAFOX_TABLE_INC

#include "table.h"
#include "def.h"

#ifdef _MSC_VER
#   pragma warning(push)
#   pragma warning(disable: 4003)
#endif // _MSC_VER

#define MAFOX_SELF Table<Types...>

#define MAFOX_TABLE(ReturnType) \
    template <typename... Types> \
    ReturnType MAFOX_EXPAND(MAFOX_SELF)

#define INLINE_MAFOX_TABLE(ReturnType) \
    template <typename... Types> \
    MAFOX_EXPAND(mafox_inline) ReturnType MAFOX_EXPAND(MAFOX_SELF)

namespace mafox
{
    namespace detail
    {
        static constexpr std::size_t DEFAULT_TABLE_SIZE = 4;

        template <typename T>
        static T *allocate(std::size_t size, std::size_t memory_size)
        {
            T *data = static_cast<T*>(malloc(memory_size * sizeof(T)));
            for(std::size_t i = 0; i < size; ++i)
                new (&data[i]) T();

            return data;
        }

        template <typename T>
        static T *allocate(std::size_t size, std::size_t memory_size, const T &value)
        {
            T *data = static_cast<T*>(malloc(memory_size * sizeof(T)));
            for(std::size_t i = 0; i < size; ++i)
                new (&data[i]) T(value);

            return data;
        }

        template <typename T>
        static T *raw_allocate(std::size_t size)
        {
            return static_cast<T*>(malloc(size * sizeof(T)));
        }

        template <typename T>
        static void destruct(T *data, std::size_t from, std::size_t to)
        {
            if(data == nullptr)
                return;

            // destructing from 'to' to 'from' for ensure the default destruction order
            for(std::size_t i = from; i < to; ++i)
                data[to - i - 1].~T();
        }

        template <typename T>
        static void free(T *data, std::size_t size)
        {
            if(data == nullptr)
                return;

            detail::destruct(data, 0, size);
            ::free(static_cast<void*>(data));
        }

        template <typename Tuple, std::size_t... INDICES>
        static auto empty_columns(std::index_sequence<INDICES...>)
        {
            return TupleType
            <
                TableColumn
                <
                    std::tuple_element_t<INDICES, Tuple>
                >...
            >
            (
                std::move
                (
                    TableColumn
                    <
                        std::tuple_element_t<INDICES, Tuple>
                    >(nullptr, 0, 0)
                )...
            );
        }

        template <typename Tuple, std::size_t... INDICES>
        static auto columns_from_rows(std::size_t rows, std::index_sequence<INDICES...>)
        {
            return TupleType
            <
                TableColumn
                <
                    std::tuple_element_t<INDICES, Tuple>
                >...
            >
            (
                std::move
                (
                    TableColumn
                    <
                        std::tuple_element_t<INDICES, Tuple>
                    >(rows)
                )...
            );
        }

        template <typename Tuple, std::size_t TYPE_INDEX>
        static auto column_from_list(std::initializer_list<Tuple> list)
        {
            using T = std::tuple_element_t<TYPE_INDEX, Tuple>;

            std::size_t size = 0;
            std::size_t memory_size = list.size() << 2;
            T *data = detail::raw_allocate<T>(memory_size);

            for(auto &&item : list)
                new (&data[size++]) T(std::get<TYPE_INDEX>(item));

            return TableColumn<T>(data, size, memory_size);
        }

        template <typename Tuple, std::size_t... INDICES>
        static auto columns_tuple_from_list(std::initializer_list<Tuple> list, std::index_sequence<INDICES...>)
        {
            return TupleType
            <
                TableColumn
                <
                    std::tuple_element_t<INDICES, Tuple>
                >...
            >
            (
                std::move(column_from_list<Tuple, INDICES>(list))...
            );
        }
    }

    template <typename T>
    mafox_inline TableColumn<T>::TableColumn()
    : data_(detail::raw_allocate<T>(detail::DEFAULT_TABLE_SIZE)), size_(0), memory_size(detail::DEFAULT_TABLE_SIZE)
    {}

    template <typename T>
    mafox_inline TableColumn<T>::TableColumn(std::size_t memory_size, DoNotConstruct)
    : data_(detail::raw_allocate<T>(memory_size)), size_(0), memory_size(memory_size)
    {}

    template <typename T>
    mafox_inline TableColumn<T>::TableColumn(const TableColumn &other)
    : data_(detail::raw_allocate<T>(other.size_ << 2)), size_(0), memory_size(other.size_ << 2)
    {
        for(; size_ < other.size_; ++size_)
            new (&data_[size_]) T(other.data_[size_]);
    }
        
    template <typename T>
    mafox_inline TableColumn<T>::TableColumn(TableColumn &&other)
    : data_(other.data_), size_(other.size_), memory_size(other.memory_size)
    {
        other.data_ = nullptr;
    }

    template <typename T>
    mafox_inline TableColumn<T>::TableColumn(std::size_t size, const T &initial_value)
    : data_(detail::allocate<T>(size, size << 2, initial_value)), size_(size), memory_size(size << 2)
    {}

    template <typename T>
    mafox_inline TableColumn<T>::TableColumn(std::size_t size)
    : data_(detail::allocate<T>(size, size << 2)), size_(size), memory_size(size << 2)
    {}

    template <typename T>
    mafox_inline TableColumn<T>::TableColumn(T *data, std::size_t size, std::size_t memory_size)
    : data_(data), size_(size), memory_size(memory_size)
    {}

    template <typename T>
    mafox_inline TableColumn<T> &TableColumn<T>::operator=(const TableColumn &rhs)
    {
        if(this != &rhs)
        {
            TableColumn copy(rhs);
            *this = std::move(copy);
        }

        return *this;
    }

    template <typename T>
    mafox_inline TableColumn<T> &TableColumn<T>::operator=(TableColumn &&rhs)
    {
        data_ = rhs.data_;
        size_ = rhs.size_;
        memory_size = rhs.memory_size;

        rhs.data_ = nullptr;
        return *this;
    }

    template <typename T>
    mafox_inline std::size_t TableColumn<T>::size() const
    {
        return size_;
    }

    template <typename T>
    mafox_inline std::size_t TableColumn<T>::capacity() const
    {
        return memory_size*sizeof(T);
    }

    template <typename T>
    mafox_inline T *TableColumn<T>::data()
    {
        return data_;
    }

    template <typename T>
    mafox_inline const T *TableColumn<T>::data() const
    {
        return const_cast<TableColumn<T>*>(this)->data();
    }

    template <typename T>
    mafox_inline T &TableColumn<T>::operator[](std::size_t index)
    {
        return data()[index];
    }

    template <typename T>
    mafox_inline const T &TableColumn<T>::operator[](std::size_t index) const
    {
        return const_cast<TableColumn<T>&>(*this)[index];
    }

    template <typename T>
    mafox_inline void TableColumn<T>::reallocate()
    {
        T *new_data = detail::raw_allocate<T>(memory_size);
        memcpy(static_cast<void*>(new_data), static_cast<void*>(data_), size_ * sizeof(T));
        detail::free(data_, size_);

        data_ = new_data;
    }

    template <typename T>
    mafox_inline void TableColumn<T>::resize(std::size_t new_size)
    {
        if(new_size > size_ && new_size > memory_size)
        {
            memory_size = new_size;
            reallocate();
        }
        else if(new_size <= size_)
            detail::destruct(data_, new_size, size_);

        size_ = new_size;
    }

    template <typename T>
    mafox_inline void TableColumn<T>::reserve(std::size_t memory_size)
    {
        if(memory_size > this->memory_size)
        {
            this->memory_size = memory_size;
            reallocate();
        }
    }

    template <typename T>
    mafox_inline void TableColumn<T>::shrink_to_fit()
    {
        memory_size = size_;
        reallocate();
    }

    template <typename T>
    mafox_inline void TableColumn<T>::add_element(const T &e)
    {
        if(size_ == memory_size)
        {
            memory_size = (memory_size == 0? detail::DEFAULT_TABLE_SIZE : memory_size << 2);
            reallocate();
        }

        new (&data_[size_++]) T(e);
    }

    template <typename T>
    mafox_inline TableColumn<T>::~TableColumn()
    {
        detail::free(data_, size_);
    }

    MAFOX_TABLE()::Table()
    {}

    MAFOX_TABLE()::Table(DoNotAllocate)
    : columns(detail::empty_columns<detail::TupleType<Types...>>(std::make_index_sequence<sizeof...(Types)>()))
    {}

    template <typename... Types>
    template <typename Tuple>
    Table<Types...>::Table(std::initializer_list<Tuple> list)
    : columns
    (
        std::move
        (
            detail::columns_tuple_from_list
            (
                list, 
                std::make_index_sequence<sizeof...(Types)>()
            )
        )
    )
    {}

    template <typename... Types>
    Table<Types...>::Table(std::size_t rows, Types&&... initial_values)
    : columns(std::move(TableColumn<Types>(rows, std::forward<Types>(initial_values)))...)
    {}

    template <typename... Types>
    Table<Types...>::Table(DoNotConstruct, detail::ReplaceWithSizeT<Types>... sizes)
    : columns(TableColumn<Types>(sizes, DO_NOT_CONSTRUCT)...)
    {}

    MAFOX_TABLE()::Table(const Table &other)
    : columns(other.columns)
    {}

    MAFOX_TABLE()::Table(Table &&other)
    : columns(std::move(other.columns))
    {}

    MAFOX_TABLE()::Table(std::size_t rows)
    : columns(detail::columns_from_rows<detail::TupleType<Types...>>(rows, std::make_index_sequence<sizeof...(Types)>()))
    {}

    INLINE_MAFOX_TABLE(MAFOX_SELF &)::operator=(const Table &rhs)
    {
        if(this != &rhs)
            columns = rhs.columns;

        return *this;
    }

    INLINE_MAFOX_TABLE(MAFOX_SELF &)::operator=(Table &&rhs)
    {
        columns = std::move(rhs.columns);
        return *this;
    }

    INLINE_MAFOX_TABLE(MAFOX_SELF &)::resize_rows(std::size_t rows)
    {
        std::apply
        (
            [&rows](auto &... columns)
            {
                (columns.resize(rows), ...);
            },
            columns
        );
    }

    INLINE_MAFOX_TABLE(std::size_t)::rows() const
    {
        return std::get<0>(columns).size();
    }

    INLINE_MAFOX_TABLE(constexpr std::size_t)::cols() const
    {
        return sizeof...(Types);
    }

    template <typename... Types>
    template <std::size_t COLUMN>
    mafox_inline auto &MAFOX_SELF::at(std::size_t row)
    {
        auto &column = std::get<COLUMN>(columns);
        
        assert(row < column.size());
        return column.data()[row];
    }

    template <typename... Types>
    template <std::size_t COLUMN>
    mafox_inline const auto &MAFOX_SELF::at(std::size_t row) const
    {
        return const_cast<MAFOX_SELF*>(this)->at<COLUMN>(row);
    }

    template <typename... Types>
    template <std::size_t COLUMN>
    mafox_inline auto &MAFOX_SELF::at()
    {
        return std::get<COLUMN>(columns);
    }

    template <typename... Types>
    template <std::size_t COLUMN>
    mafox_inline const auto &MAFOX_SELF::at() const
    {
        return const_cast<MAFOX_SELF*>(this)->at<COLUMN>();
    }

    INLINE_MAFOX_TABLE(MAFOX_SELF &)::add_row(const Types&... args)
    {
        std::apply
        (
            [&](auto &... columns)
            {
                (columns.add_element(args), ...);
            },
            columns
        );

        return *this;
    }

    INLINE_MAFOX_TABLE(MAFOX_SELF &)::add_row(Types&&... args)
    {
        std::apply
        (
            [&](auto &... columns)
            {
                (columns.add_element(std::move(args)), ...);
            },
            columns
        );

        return *this;
    }

    template <typename... Types>
    template <template <typename...> typename TupleT>
    mafox_inline MAFOX_SELF &MAFOX_SELF::add_row(const TupleT<Types...> &args)
    {
        std::apply
        (
            [&](auto &... args)
            {
                this->add_row(args...);
            },
            args
        );

        return *this;
    }

    template <typename... Types>
    template <template <typename...> typename TupleT>
    mafox_inline MAFOX_SELF &MAFOX_SELF::add_row(TupleT<Types...> &&args)
    {
        std::apply
        (
            [&](auto &... args)
            {
                this->add_row(std::move(args)...);
            },
            args
        );

        return *this;
    }

    INLINE_MAFOX_TABLE(void)::shrink_to_fit()
    {
        std::apply
        (
            [&](auto &... columns)
            {
                (columns.shrink_to_fit(), ...);
            },
            columns
        );
    }
}

namespace std
{
    template <std::size_t INDEX, typename... Types>
    auto &get(mafox::Table<Types...> &table)
    {
        return table.template at<INDEX>();
    }

    template <std::size_t INDEX, typename... Types>
    const auto &get(const mafox::Table<Types...> &table)
    {
        return table.template at<INDEX>();
    }
}

#ifdef _MSC_VER
#   pragma warning(pop)
#endif // _MSC_VER

#undef INLINE_MAFOX_TABLE
#undef MAFOX_GMATRIX
#undef MAFOX_SELF

#endif // MAFOX_TABLE_INC