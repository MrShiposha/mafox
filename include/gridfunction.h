#ifndef MAFOX_GRIDFUNCTION_H
#define MAFOX_GRIDFUNCTION_H

#include <initializer_list>

#include "def.h"
#include "table.h"

namespace mafox
{
    namespace detail
    {
        template <typename... Args>
        using TupleT = metaxxa::Tuple<Args...>;

        template <typename... Args>
        struct GridNodeArgs
        {
            mafox_inline explicit GridNodeArgs(Args&&... args);

            template <typename Value>
            mafox_inline TupleT<Value, Args...> operator=(const Value &) const;

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
        using FunctionType = Value(Args...);

        template <template <typename...> typename Template>
        using MoveFunctionTypes = Template<Value, Args...>;

        using Result = Value;

        template <template <typename...> typename Template>
        using MoveFunctionArgTypes = Template<Args...>;

        template <std::size_t INDEX>
        using Argument = typename MoveFunctionArgTypes<metaxxa::TypeTuple>::template Get<INDEX>;

        mafox_inline GridFunction();

        mafox_inline GridFunction(std::initializer_list<detail::TupleT<Value, Args...>>);

        mafox_inline std::size_t nodes_count() const;

    private:
        Table<Value, Args...> table;
        std::size_t nodes_count_;
    };

    template <typename Value, typename... Args>
    GridFunction(std::initializer_list<detail::TupleT<Value, Args...>>) 
        -> GridFunction
        <
            metaxxa::MakeFunctionType
            <
                metaxxa::TypeTuple<Value, Args...>, 
                0
            >
        >; 
}

#endif // MAFOX_GRIDFUNCTION_H