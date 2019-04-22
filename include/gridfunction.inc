#ifndef MAFOX_GRIDFUNCTION_INC
#define MAFOX_GRIDFUNCTION_INC

#include "gridfunction.h"

namespace mafox
{
    namespace detail
    {
        template <typename Value, typename... Args>
        struct ValueCatter
        {
            template <std::size_t... INDICES>
            static TupleT<Value, Args...> cat(std::index_sequence<INDICES...>, const Value &value, const TupleT<Args...> &args)
            {
                TupleT<Value, Args...> result(value, std::get<INDICES>(args)...);

                return result;
            };
        };

        template <typename... Args>
        mafox_inline GridNodeArgs<Args...>::GridNodeArgs(Args&&... args)
        : args(std::forward<Args>(args)...)
        {}

        template <typename... Args>
        template <typename Value>
        mafox_inline TupleT<Value, Args...> GridNodeArgs<Args...>::operator=(const Value &value) const
        {
            return ValueCatter<Value, Args...>::cat(std::make_index_sequence<sizeof...(Args)>(), value, args);
        }
    }

    template <typename... Args>
    mafox_inline detail::GridNodeArgs<Args...> f(Args&&... args)
    {
        return detail::GridNodeArgs<Args...>(std::forward<Args>(args)...);
    }

    template <typename Value, typename... Args>
    mafox_inline GridFunction<Value(Args...)>::GridFunction()
    : table(), nodes_count_(0)
    {}

    template <typename Value, typename... Args>
    mafox_inline GridFunction<Value(Args...)>::GridFunction(std::initializer_list<detail::TupleT<Value, Args...>> list)
    : table(list), nodes_count_(list.size())
    {}

    template <typename Value, typename... Args>
    mafox_inline std::size_t GridFunction<Value(Args...)>::nodes_count() const
    {
        return nodes_count_;
    }

    // TODO: deduction guides for GridFunction
}

#endif // MAFOX_GRIDFUNCTION_INC