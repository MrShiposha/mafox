// MIT License
// 
// Copyright (c) 2018 Daniel Shiposha
// 
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
// 
// The above copyright notice and this permission notice shall be included in all
// copies or substantial portions of the Software.
// 
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.
// 

#ifndef METAXXA_HPP
#define METAXXA_HPP

#include <type_traits>
#include <utility>

#ifndef METAXXA_ISVALID_H
#define METAXXA_ISVALID_H



#ifndef METAXXA_TYPESAVER_H
#define METAXXA_TYPESAVER_H

namespace metaxxa
{
    template <typename T>
    struct TypeSaver
    {
        using Type = T;
    };
}

#endif // METAXXA_TYPESAVER_H

namespace metaxxa
{
    namespace detail
    {
        template <typename... Args>
        class IsValid
        {
            template <typename Callable>
            static constexpr auto check(...) 
                -> std::false_type;

            template <typename Callable>
            static constexpr auto check(TypeSaver<decltype(std::declval<Callable&&>()(std::declval<Args&&>()...))> *)
                -> std::true_type;

        public:
            template <typename Callable>
            constexpr bool operator()(Callable &&) const
            {
                return std::is_same_v<decltype(check<Callable>(0)), std::true_type>;
            }
        };
    }

    template <typename... Types>
    constexpr detail::IsValid<Types...> is_valid {}; 
}

#endif // METAXXA_ISVALID_H

#ifndef METAXXA_STATIC_TUPLE_H
#define METAXXA_STATIC_TUPLE_H


#ifndef METAXXA_STATICLIST_H
#define METAXXA_STATICLIST_H


namespace metaxxa
{
    namespace detail
    {
        class ListTag {};
    }

    template <typename... Args>
    class List;

    using Nil = List<>;

    template <typename H = Nil, typename... Tail>
    struct CarT
    {
        using Head = H;
    };

    template <typename H = Nil, typename... Args>
    struct CdrT
    {
        using Tail = List<Args...>;
    };

    template <typename T>
    struct CdrT<T>
    {
        using Tail = Nil;
    };

    template <typename... Args>
    using Car = typename CarT<Args...>::Head;

    template <typename... Args>
    using Cdr = typename CdrT<Args...>::Tail;

    template <typename... Args>
    class List : public detail::ListTag
    {
    public:
        constexpr List() = default;

        using Head = Car<Args...>;
        using Tail = Cdr<Args...>;
    };
}

namespace std
{
    template <typename... Args>
    class tuple_size<metaxxa::List<Args...>>
        : public std::integral_constant<std::size_t, sizeof...(Args)>
    {};

    template <>
    class tuple_size<metaxxa::List<>>
        : public std::integral_constant<std::size_t, 0>
    {};

    template <size_t INDEX, typename... Args>
	class tuple_element<INDEX, metaxxa::List<Args...>>
        : public tuple_element<INDEX - 1, typename metaxxa::List<Args...>::Tail>
	{};

    template <size_t INDEX>
	class tuple_element<INDEX, metaxxa::Nil>
    {

    };

    template <>
	class tuple_element<0, metaxxa::Nil>
    {};

    template <typename... Args>
	class tuple_element<0, metaxxa::List<Args...>>
	{
	public:
		using type = typename metaxxa::List<Args...>::Head;
	};
}

#endif // METAXXA_STATICLIST_H

namespace metaxxa
{
    template <typename... Args>
    class StaticTuple;

    namespace detail
    {
        template <typename... Args>
        struct TupleConcatenator
        {
            template <typename... RHSArgs>
            static constexpr auto result_tuple(StaticTuple<RHSArgs...> &&) 
                -> StaticTuple<Args..., RHSArgs...>;
        };
    }

    template <typename... Args>
    class StaticTuple : public List<Args...>
    {
    public:
        using List = metaxxa::List<Args...>;

        template <std::size_t INDEX>
        using Get = typename std::tuple_element_t<INDEX, List>;

        template <typename RHSTuple>
        using Concat = decltype(detail::TupleConcatenator<Args...>::template result_tuple(std::declval<RHSTuple>()));

        constexpr StaticTuple() = default;

        static constexpr bool is_empty();

        static constexpr std::size_t get_size();

        static constexpr std::size_t size();
    };
}

namespace std
{
    template <typename... Args>
    class tuple_size<metaxxa::StaticTuple<Args...>>
        : public std::integral_constant<std::size_t, sizeof...(Args)>
    {};

    template <>
    class tuple_size<metaxxa::StaticTuple<>>
        : public std::integral_constant<std::size_t, 0>
    {};

    template <size_t INDEX, typename... Args>
	class tuple_element<INDEX, metaxxa::StaticTuple<Args...>>
	{
	public:
		using type = tuple_element_t<INDEX, typename metaxxa::StaticTuple<Args...>::List>;
	};
}

#endif // METAXXA_STATIC_TUPLE_H


namespace metaxxa
{
    template <typename... Args>
    constexpr bool StaticTuple<Args...>::is_empty()
    {
        return std::is_same_v<Car<Args...>, Nil>;
    }

    template <typename... Args>
    constexpr std::size_t StaticTuple<Args...>::get_size()
    {
        return std::tuple_size_v<StaticTuple<Args...>>;
    }

    template <typename... Args>
    constexpr std::size_t StaticTuple<Args...>::size()
    {
        return get_size();
    }
}


#ifndef METAXXA_TYPEIF_H
#define METAXXA_TYPEIF_H


namespace metaxxa
{
    namespace detail
    {
        template <bool CONDITION, typename Then, typename Else>
        struct ElseResolver
        {
            using Type = Then;
        };

        template <typename Then, typename Else>
        struct ElseResolver<false, Then, Else>
        {
            using Type = Else;
        };

        template <bool CONDITION, typename T>
        struct ThenResolver
        {
            template <typename E>
            using Else = ElseResolver<CONDITION, T, E>;
        };

        template <typename T>
        struct ThenResolver<true, T>
        {
            using Type = T;

            template <typename E>
            using Else = ElseResolver<true, T, E>;
        };
    }

    template <bool CONDITION>
    struct TypeIf
    {
        template <typename T>
        using Then = detail::ThenResolver<CONDITION, T>;
    };
}

#endif // METAXXA_TYPEIF_H

#ifndef METAXXA_MINIMALARGUMENT_H
#define METAXXA_MINIMALARGUMENT_H


namespace metaxxa
{
    template <typename T>
    using MinimalArgument = typename 
        TypeIf<sizeof(T) <= sizeof(T *)>
                ::template Then<T>
                ::template Else<const T &>
                ::Type;
}

#endif // METAXXA_MINIMALARGUMENT_H


#ifndef METAXXA_TIMES_H
#define METAXXA_TIMES_H

namespace metaxxa
{
    namespace detail
    {
        template
        <
            template <typename...> typename Template,
            std::size_t N,
            typename T,
            typename... Ts
        >
        struct Times : Times<Template, N - 1, T, T, Ts...>
        {};

        template
        <
            template <typename...> typename Template,
            typename T,
            typename... Ts
        >
        struct Times<Template, 0, T, Ts...>
        {
            using Type = Template<Ts...>;
        };
    }

    template
    <
        template <typename...> typename Template,
        std::size_t N,
        typename T
    >
    using Times = typename detail::Times<Template, N, T>::Type;
}

#endif // METAXXA_TIMES_H

#ifndef METAXXA_MOVEPARAMETERS_H
#define METAXXA_MOVEPARAMETERS_H

namespace metaxxa
{
    namespace detail
    {
        template
        <
            template <typename...> typename DestTemplate,
            template <typename...> typename SrcTemplate,
            typename... Args
        >
        constexpr auto move_parameters(SrcTemplate<Args...> &&) -> DestTemplate<Args...>;
    }

    template 
    <
        template <typename...> typename DestTemplate,
        typename SrcTemplate
    >
    using MoveParameters = decltype(detail::move_parameters<DestTemplate>(std::declval<SrcTemplate>()));
}

#endif // METAXXA_MOVEPARAMETERS_H

#ifndef METAXXA_PARAMETERSCOUNT_H
#define METAXXA_PARAMETERSCOUNT_H


namespace metaxxa
{
    namespace detail
    {
        template
        <
            template <typename...> typename Template,
            typename... Args
        >
        constexpr auto parameters_count(Template<Args...> &&)
            -> std::integral_constant<std::size_t, sizeof...(Args)>;
    }

    // Holds number of template parameters of a class template
    template <typename T>
    using ParametersCount = decltype(detail::parameters_count(std::declval<T>()));

    template <typename T>
    constexpr std::size_t parameters_count() 
    {
        return ParametersCount<T>::value;
    }
}

#endif // METAXXA_PARAMETERSCOUNT_H


#ifndef METAXXA_INDEXRANGE_H
#define METAXXA_INDEXRANGE_H


namespace metaxxa
{
    namespace detail
    {
        template <std::size_t TO_ADD, std::size_t... SEQ>
        constexpr auto shift_seq(std::index_sequence<SEQ...> &&)
            -> std::index_sequence<TO_ADD + SEQ ...>;
    }

    template <std::size_t MIN, std::size_t MAX>
    using MakeIndexRange = decltype(detail::shift_seq<MIN>(std::declval<std::make_index_sequence<MAX-MIN>>())); 
}

#endif // METAXXA_INDEXRANGE_H


#ifndef METAXXA_ALGORITHM_H
#define METAXXA_ALGORITHM_H


#ifndef METAXXA_ALGORITHM_INDEXFILTER_H
#define METAXXA_ALGORITHM_INDEXFILTER_H


namespace metaxxa
{
    template 
    <
        template <typename...> typename Template, 
        typename TupleT, 
        std::size_t... INDICES
    >
    using IndexFilter = Template<std::tuple_element_t<INDICES, TupleT>...>;

}

#endif // METAXXA_ALGORITHM_INDEXFILTER_H

#ifndef METAXXA_ALGORITHM_SEQFILTER_H
#define METAXXA_ALGORITHM_SEQFILTER_H


namespace metaxxa
{
    namespace detail
    {
        template
        <
            template <typename...> typename Template,
            typename TupleT,
            std::size_t... INDICES
        >
        constexpr auto seq_filter(std::index_sequence<INDICES...> &&)
            -> IndexFilter<Template, TupleT, INDICES...>;
    }

    template
    <
        template <typename...> typename Template,
        typename TupleT,
        typename Seq
    >
    using SeqFilter = decltype(detail::seq_filter<Template, TupleT>(std::declval<Seq>()));
}

#endif // METAXXA_ALGORITHM_SEQFILTER_H


#ifndef METAXXA_ALGORITHM_SKIPRANGE_H
#define METAXXA_ALGORITHM_SKIPRANGE_H



namespace metaxxa
{
    template
    <
        template <typename...> typename Template,
        typename TupleT,
        std::size_t FROM_I,
        std::size_t TO_I
    >
    using SkipRange = SeqFilter
    <
        Template,
        TupleT,
        MakeIndexRange<FROM_I, TO_I>
    >;
}

#endif // METAXXA_ALGORITHM_SKIPRANGE_H

#ifndef METAXXA_ALGORITHM_SKIPFIRST_H
#define METAXXA_ALGORITHM_SKIPFIRST_H


namespace metaxxa
{
    template 
    <
        template <typename...> typename Template,
        typename TupleT, 
        std::size_t N
    >
    using SkipFirst = SkipRange
    <
        Template,
        TupleT,
        N, std::tuple_size_v<TupleT>
    >;
}

#endif // METAXXA_ALGORITHM_SKIPFIRST_H

#ifndef METAXXA_ALGORITHM_SKIPLAST_H
#define METAXXA_ALGORITHM_SKIPLAST_H


namespace metaxxa
{
    template
    <
        template <typename...> typename Template,
        typename TupleT,
        std::size_t N
    >
    using SkipLast = SkipRange
    <
        Template,
        TupleT,
        0, std::tuple_size_v<TupleT> - N
    >;
}

#endif // METAXXA_ALGORITHM_SKIPLAST_H

#endif // METAXXA_ALGORITHM_H


#endif // METAXXA_HPP