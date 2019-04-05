#ifndef MAFOX_GRIDFUNCTION_INC
#define MAFOX_GRIDFUNCTION_INC

#include "gridfunction.h"

namespace mafox
{
    namespace detail
    {
        template <typename... Args>
        mafox_inline GridNodeArgs<Args...>::GridNodeArgs(Args&&... args)
        : args(std::forward<Args>(args)...)
        {}

        template <typename... Args>
        template <typename Value>
        mafox_inline TupleT<Args..., Value> GridNodeArgs<Args...>::operator=(const Value &value) const
        {
            return std::tuple_cat(args, TupleT<Value>(value));
        }
    }

    template <typename... Args>
    mafox_inline detail::GridNodeArgs<Args...> f(Args&&... args)
    {
        return detail::GridNodeArgs<Args...>(std::forward<Args>(args)...);
    }

    template <typename Value, typename... Args>
    mafox_inline GridFunction<Value(Args...)>::GridFunction()
    {}

    template <typename Value, typename... Args>
    mafox_inline GridFunction<Value(Args...)>::GridFunction(std::initializer_list<detail::TupleT<Args..., Value>> list)
    : nodes(list)
    {}

    template <typename Value, typename... Args>
    mafox_inline auto &GridFunction<Value(Args...)>::node(std::size_t index)
    {
        assert(index < nodes.size());

        return nodes[index];
    }

    template <typename Value, typename... Args>
    mafox_inline const auto &GridFunction<Value(Args...)>::node(std::size_t index) const
    {
        return const_cast<GridFunction<Value(Args...)>*>(this)->node(index);
    }

    template <typename Value, typename... Args>
    mafox_inline std::size_t GridFunction<Value(Args...)>::nodes_count() const
    {
        return nodes.size();
    }

    // TODO: deduction guides for GridFunction
}

#endif // MAFOX_GRIDFUNCTION_INC