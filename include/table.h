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
        using TupleType = std::tuple<Types...>;
    }

    template <typename T>
    class TableColumn
    {
    public:
        mafox_inline TableColumn();

        mafox_inline TableColumn(const TableColumn &);
        
        mafox_inline TableColumn(TableColumn &&);

        mafox_inline TableColumn(std::size_t);

        mafox_inline TableColumn(std::size_t, const T &initial_value);

        mafox_inline TableColumn(T *, std::size_t, std::size_t);

        mafox_inline ~TableColumn();

        mafox_inline TableColumn &operator=(const TableColumn &);

        mafox_inline TableColumn &operator=(TableColumn &&);

        mafox_inline std::size_t size() const;

        mafox_inline T *data();

        mafox_inline const T *data() const;

        mafox_inline void reallocate();

        mafox_inline void resize(std::size_t);

        mafox_inline void shrink_to_fit();

        mafox_inline void add_element(const T &);

    private:
        T *data_;
        std::size_t size_;
        std::size_t memory_size;
    };

    template <typename... Types>
    class Table
    {
    public:
        Table();

        template <template <typename...> typename TupleT>
        Table(std::initializer_list<TupleT<Types...>>);

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