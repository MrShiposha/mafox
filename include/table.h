#ifndef MAFOX_TABLE_H
#define MAFOX_TABLE_H

#include <memory>
#include <initializer_list>
#include <type_traits>
#include <tuple> // Temporary

namespace mafox
{
    namespace detail
    {
        template <typename... Types>
        using TupleType = metaxxa::Tuple<Types...>;

        template <typename>
        using ReplaceWithSizeT = std::size_t;

        template <typename... Args>
        inline constexpr bool is_not_size_t()
        {
            return 
            (
                true && ... && !std::is_same_v<Args, size_t>
            );
        }

        template <typename T, bool IS_VALID>
        struct AllowOnlyTuples
        {
            using Type = T;
        };

        template <typename T>
        struct AllowOnlyTuples<T, false>
        {};
    }

    struct DoNotConstruct {};

    inline constexpr DoNotConstruct DO_NOT_CONSTRUCT {};

    template <typename T>
    class TableColumn
    {
    public:
        mafox_inline TableColumn();

        mafox_inline TableColumn(std::size_t memory_size, DoNotConstruct);

        mafox_inline TableColumn(const TableColumn &);
        
        mafox_inline TableColumn(TableColumn &&);

        mafox_inline TableColumn(std::size_t size);

        mafox_inline TableColumn(std::size_t size, const T &initial_value);

        mafox_inline TableColumn(T *data, std::size_t size, std::size_t memory_size);

        mafox_inline ~TableColumn();

        mafox_inline TableColumn &operator=(const TableColumn &);

        mafox_inline TableColumn &operator=(TableColumn &&);

        mafox_inline std::size_t size() const;

        mafox_inline std::size_t capacity() const;

        mafox_inline T *data();

        mafox_inline const T *data() const;

        mafox_inline T &operator[](std::size_t);

        mafox_inline const T &operator[](std::size_t) const;

        mafox_inline void reallocate();

        mafox_inline void resize(std::size_t);

        mafox_inline void reserve(std::size_t memory_size);

        mafox_inline void shrink_to_fit();

        mafox_inline void add_element(const T &);

    private:
        T *data_;
        std::size_t size_;
        std::size_t memory_size;
    };

    struct DoNotAllocate {};

    inline constexpr DoNotAllocate DO_NOT_ALLOCATE {};

    template <typename... Types>
    class Table
    {
    public:
        Table();

        Table(DoNotAllocate);

        template <typename Tuple>
        Table(std::initializer_list<Tuple>);

        template <typename... Tuples, typename = std::enable_if_t<detail::is_not_size_t<Tuples...>()>>
        Table(Tuples&&...);

        template <typename... Tuples, typename = std::enable_if_t<detail::is_not_size_t<Tuples...>()>>
        Table(const Tuples&...);

        Table(DoNotConstruct, detail::ReplaceWithSizeT<Types>... memory_sizes);

        Table(std::size_t rows, Types&&... initial_values);

        Table(const Table &);

        Table(Table &&);

        Table(std::size_t rows);

        Table &operator=(const Table &);

        Table &operator=(Table &&);

        mafox_inline Table &resize_rows(std::size_t rows);

        mafox_inline std::size_t rows() const;

        mafox_inline constexpr std::size_t cols() const;

        template <std::size_t COLUMN>
        mafox_inline auto &at(std::size_t row);

        template <std::size_t COLUMN>
        mafox_inline const auto &at(std::size_t row) const;

        template <std::size_t COLUMN>
        mafox_inline auto &at();

        template <std::size_t COLUMN>
        mafox_inline const auto &at() const;

        mafox_inline Table &add_row(const Types & ...);

        mafox_inline Table &add_row(Types && ...);

        template <template <typename...> typename TupleT>
        mafox_inline Table &add_row(const TupleT<Types...> &);

        template <template <typename...> typename TupleT>
        mafox_inline Table &add_row(TupleT<Types...> &&);

        mafox_inline void shrink_to_fit();

    private:
        detail::TupleType<TableColumn<Types>...> columns;
    };

    template <>
    class Table<>
    {};
}

namespace std
{
    template <std::size_t INDEX, typename... Types>
    class tuple_element<INDEX, mafox::Table<Types...>>
    {
    public:
        using type = std::tuple_element_t<INDEX, metaxxa::TypeList<Types...>>;
    };

    template <std::size_t INDEX, typename... Types>
    auto &get(mafox::Table<Types...> &);

    template <std::size_t INDEX, typename... Types>
    const auto &get(const mafox::Table<Types...> &); 
}

#endif // MAFOX_TABLE_H