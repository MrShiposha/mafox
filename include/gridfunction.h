#ifndef MAFOX_GRIDFUNCTION_H
#define MAFOX_GRIDFUNCTION_H

#include "def.h"

#include <tuple>
#include <vector>
#include <initializer_list>

namespace mafox
{
    namespace detail
    {
        template <typename... Args>
        using TupleT = std::tuple<Args...>;

        template <typename T>
        using TupleContainerT = std::vector<T>;

        template <typename... Args>
        struct GridNodeArgs
        {
            mafox_inline explicit GridNodeArgs(Args&&... args);

            template <typename Value>
            mafox_inline TupleT<Args..., Value> operator=(const Value &) const;

            TupleT<Args...> args;
        };
    }
    
    template <typename... Args>
    mafox_inline detail::GridNodeArgs<Args...> f(Args&&... args);

    template <typename Fn>
    class GridFunction : public GridFunction<decltype(&Fn::operator())>
    {};

    template <typename Value, typename... Args>
    class GridFunction<Value(Args...)>
    {
    public:
        mafox_inline GridFunction();

        mafox_inline GridFunction(std::initializer_list<detail::TupleT<Args..., Value>>);

        mafox_inline auto &node(std::size_t index);

        mafox_inline const auto &node(std::size_t index) const;

        mafox_inline std::size_t nodes_count() const;


    private:
        detail::TupleContainerT<detail::TupleT<Args..., Value>> nodes;
    };
}

#endif // MAFOX_GRIDFUNCTION_H