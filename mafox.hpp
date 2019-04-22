#ifndef MAFOX_HPP
#define MAFOX_HPP

#include <type_traits>
#include <functional>
#include <utility>
#include <cstring>
#include <cstddef>
#include <cstdint>
#include <memory>
#include <vector>
#include <future>
#include <initializer_list>
#include <tuple> // Temporary

#ifndef MAFOX_H
#define MAFOX_H


#ifndef MAFOX_DETAIL_DEF_H
#define MAFOX_DETAIL_DEF_H



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

#ifndef METAXXA_TYPELIST_H
#define METAXXA_TYPELIST_H


namespace metaxxa
{
    namespace detail
    {
        class ListTag {};
    }

    template <typename... Args>
    class TypeList;

    using Nil = TypeList<>;

    template <typename H = Nil, typename... Tail>
    struct CarT
    {
        using Head = H;
    };

    template <typename H = Nil, typename... Args>
    struct CdrT
    {
        using Tail = TypeList<Args...>;
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
    class TypeList : public detail::ListTag
    {
    public:
        constexpr TypeList() = default;

        using Head = Car<Args...>;
        using Tail = Cdr<Args...>;
    };
}

namespace std
{
    template <typename... Args>
    class tuple_size<metaxxa::TypeList<Args...>>
        : public std::integral_constant<std::size_t, sizeof...(Args)>
    {};

    template <>
    class tuple_size<metaxxa::TypeList<>>
        : public std::integral_constant<std::size_t, 0>
    {};

    template <size_t INDEX, typename... Args>
	class tuple_element<INDEX, metaxxa::TypeList<Args...>>
        : public tuple_element<INDEX - 1, typename metaxxa::TypeList<Args...>::Tail>
	{};

    template <size_t INDEX>
	class tuple_element<INDEX, metaxxa::Nil>
    {

    };

    template <>
	class tuple_element<0, metaxxa::Nil>
    {};

    template <typename... Args>
	class tuple_element<0, metaxxa::TypeList<Args...>>
	{
	public:
		using type = typename metaxxa::TypeList<Args...>::Head;
	};
}

#endif // METAXXA_TYPELIST_H

#ifndef METAXXA_LIETRALLIST_H
#define METAXXA_LIETRALLIST_H



#ifndef METAXXA_LITERAL_H
#define METAXXA_LITERAL_H

namespace metaxxa
{
    template <typename T, T LITERAL>
    struct Literal
    {
        using Type = T;

        static constexpr Type value() { return LITERAL; }
    };

}

#endif // METAXXA_LITERAL_H

namespace metaxxa
{
    namespace detail
    {
        struct LiteralListTag
        {};
    }

    struct LiteralNilT
    {};

    inline static constexpr LiteralNilT NIL {};

    template <typename T = LiteralNilT, T... LITERALS>
    struct LiteralList;

    using LiteralNil = LiteralList<>;

    template <typename LiteralH = LiteralNil, typename... LiteralTail>
    struct LiteralCarT
    {
        using Head = LiteralH;
    };

    template <typename LiteralH = LiteralNil, typename... LiteralTail>
    struct LiteralCdrT
    {
        using Tail = LiteralList<typename LiteralH::Type, LiteralTail::value()...>;
    };

    template <typename LiteralT>
    struct LiteralCdrT<LiteralT>
    {
        using Tail = LiteralNil;
    };

    template <typename... Literals>
    using LiteralCar = typename LiteralCarT<Literals...>::Head;

    template <typename... Literals>
    using LiteralCdr = typename LiteralCdrT<Literals...>::Tail;

    template <typename T, T... LITERALS>
    struct LiteralList : public detail::LiteralListTag
    {
        using Head = LiteralCar<Literal<T, LITERALS>...>;
        using Tail = LiteralCdr<Literal<T, LITERALS>...>;

        using HeadType = typename Head::Type;

        static constexpr HeadType head()
        {
            return Head::value();
        }
    };

    template <typename T>
    struct LiteralList<T> : public detail::LiteralListTag
    {
        using Head = LiteralNil;
        using Tail = LiteralNil;

        using HeadType = LiteralNilT;

        static constexpr HeadType head()
        {
            return NIL;
        }
    };
}

template <typename T>
constexpr bool operator==(const T, const metaxxa::LiteralNilT)
{
    return false;
}

template <typename T>
constexpr bool operator==(const metaxxa::LiteralNilT, const T)
{
    return false;
}

constexpr bool operator==(const metaxxa::LiteralNilT, const metaxxa::LiteralNilT)
{
    return true;
}

#endif // METAXXA_LIETRALLIST_H


#ifndef METAXXA_TYPETUPLE_H
#define METAXXA_TYPETUPLE_H


#ifndef METAXXA_CONCAT_H
#define METAXXA_CONCAT_H


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
        constexpr auto move_parameters(SrcTemplate<Args...> &&) -> TypeSaver<DestTemplate<Args...>>;
    }

    template 
    <
        template <typename...> typename DestTemplate,
        typename SrcTemplate
    >
    using MoveParameters = typename decltype
    (
        detail::move_parameters<DestTemplate>(std::declval<SrcTemplate>())
    )::Type;
}

#endif // METAXXA_MOVEPARAMETERS_H

#ifndef METAXXA_TEMPLATECONTAINER_H
#define METAXXA_TEMPLATECONTAINER_H

namespace metaxxa
{
    template <typename... Args>
    struct TemplateContainer 
    {};
}

#endif // METAXXA_TEMPLATECONTAINER_H

namespace metaxxa
{
    namespace detail
    {
        template
        <
            typename RHS,
            typename... LHSArgs
        >
        struct Concatenator
        {
            template <typename... RHSArgs>
            static constexpr auto evaltype(TemplateContainer<RHSArgs...> &&)
                -> TemplateContainer<LHSArgs..., RHSArgs...>;

            using RHSTypeList = MoveParameters<TemplateContainer, RHS>;
            using Type = decltype(evaltype(std::declval<RHSTypeList>()));
        };

        template
        <
            typename LHS,
            typename RHS,
            bool IS_RHS_LIST = std::is_base_of_v<ListTag, RHS>
        >
        struct ConcatenatorImpl
        {
            template <typename... LHSArgs>
            static constexpr auto evaltype(TemplateContainer<LHSArgs...> &&)
                -> Concatenator<RHS, LHSArgs...>;

            using LHSTypeList = MoveParameters<TemplateContainer, LHS>;
            using Type = typename decltype(evaltype(std::declval<LHSTypeList>()))::Type;
        };

        template <typename LHS, bool IS_LIST = std::is_base_of_v<ListTag, LHS>>
        struct ResolveEndType
        {
            using Type = MoveParameters<TemplateContainer, LHS>;
        };

        template <typename LHS>
        struct ResolveEndType<LHS, false>
        {
            using Type = LHS;
        };

        template
        <
            typename LHS
        >
        struct ConcatenatorImpl<LHS, TypeList<>, true>
        {
            using Type = typename ResolveEndType<LHS>::Type;
        };

        template
        <
            typename LHS,
            typename RHS
        >
        struct ConcatenatorImpl<LHS, RHS, true>
        {
            using Type = typename ConcatenatorImpl
            <
                LHS,
                typename ConcatenatorImpl
                <
                    typename RHS::Head,
                    typename RHS::Tail
                >::Type
            >::Type;
        };

        template
        <
            template <typename...> typename Template,
            typename... Templates
        >
        struct ConcatenatorFacade
        {
            using List = TypeList<Templates...>;

            using Type = MoveParameters
            <
                Template,
                typename ConcatenatorImpl
                <
                    typename List::Head,
                    typename List::Tail
                >::Type
            >;
        };
    }

    template
    <
        template <typename...> typename Template,
        typename... Templates
    >
    using Concat = typename detail::ConcatenatorFacade<Template, Templates...>::Type;
}

#endif // METAXXA_CONCAT_H

namespace metaxxa
{
    template <typename... Args>
    class TypeTuple
    {
    public:
        using List = metaxxa::TypeList<Args...>;

        template <std::size_t INDEX>
        using Get = typename std::tuple_element_t<INDEX, List>;

        template <typename... RHS>
        using Concat = Concat<metaxxa::TypeTuple, TypeTuple, RHS...>;

        constexpr TypeTuple() = default;

        static constexpr bool is_empty();

        static constexpr std::size_t get_size();

        static constexpr std::size_t size();
    };
}

namespace std
{
    template <typename... Args>
    class tuple_size<metaxxa::TypeTuple<Args...>>
        : public std::integral_constant<std::size_t, sizeof...(Args)>
    {};

    template <>
    class tuple_size<metaxxa::TypeTuple<>>
        : public std::integral_constant<std::size_t, 0>
    {};

    template <size_t INDEX, typename... Args>
	class tuple_element<INDEX, metaxxa::TypeTuple<Args...>>
	{
	public:
		using type = tuple_element_t<INDEX, typename metaxxa::TypeTuple<Args...>::List>;
	};
}

#endif // METAXXA_TYPETUPLE_H


namespace metaxxa
{
    template <typename... Args>
    constexpr bool TypeTuple<Args...>::is_empty()
    {
        return std::is_same_v<Car<Args...>, Nil>;
    }

    template <typename... Args>
    constexpr std::size_t TypeTuple<Args...>::get_size()
    {
        return std::tuple_size_v<TypeTuple<Args...>>;
    }

    template <typename... Args>
    constexpr std::size_t TypeTuple<Args...>::size()
    {
        return get_size();
    }
}


#ifndef METAXXA_IF_H
#define METAXXA_IF_H


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
    struct If
    {
        template <typename T>
        using Then = detail::ThenResolver<CONDITION, T>;
    };
}

#endif // METAXXA_IF_H

#ifndef METAXXA_TYPEIF_H
#define METAXXA_TYPEIF_H



namespace metaxxa
{
    template <typename T>
    using TypeIf = If<!std::is_same_v<T, std::false_type>>;
}

#endif // METAXXA_TYPEIF_H

#ifndef METAXXA_TYPEORREF_H
#define METAXXA_TYPEORREF_H



namespace metaxxa
{
    template <typename T>
    using TypeOrRef = typename 
        If<sizeof(T) <= sizeof(T *)>
                ::template Then<T>
                ::template Else<T &>
                ::Type;

    template <typename T>
    using TypeOrRefWrapper = typename 
        If<sizeof(T) <= sizeof(T *)>
                ::template Then<T>
                ::template Else<std::reference_wrapper<T>>
                ::Type;

    template <typename T>
    using TypeOrRefConstWrapper = typename 
        If<sizeof(T) <= sizeof(T *)>
                ::template Then<T>
                ::template Else<std::reference_wrapper<const T>>
                ::Type;

    template <typename T>
    auto obj_or_ref(T &obj)
    {
        return TypeOrRefWrapper<T>(obj);
    }

    template <typename T>
    auto obj_or_cref(const T &obj)
    {
        return TypeOrRefConstWrapper<T>(obj);
    }
}

#endif // METAXXA_TYPEORREF_H


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

#ifndef METAXXA_PARAMETERSCOUNT_H
#define METAXXA_PARAMETERSCOUNT_H



#ifndef METAXXA_SIZECONSTANT_H
#define METAXXA_SIZECONSTANT_H



#ifndef METAXXA_VALUEMETHOD_H
#define METAXXA_VALUEMETHOD_H

namespace metaxxa
{
    template <typename T>
    struct ValueMethod
    {
        using Type = typename T::value_type;

        static constexpr Type value() { return T::value; }
    };
}

#endif // METAXXA_VALUEMETHOD_H

namespace metaxxa
{
    template <std::size_t INDEX>
    using SizeConstant = ValueMethod<std::integral_constant<std::size_t, INDEX>>;
}

#endif // METAXXA_SIZECONSTANT_H

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
            -> SizeConstant<sizeof...(Args)>;
    }

    // Holds number of template parameters of a class template
    template <typename T>
    using ParametersCount = decltype(detail::parameters_count(std::declval<T>()));

    template <typename T>
    constexpr std::size_t parameters_count() 
    {
        return ParametersCount<T>::value();
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
        constexpr auto shift_seq_plus(std::index_sequence<SEQ...> &&)
            -> std::index_sequence<TO_ADD + SEQ ...>;

        template <std::size_t TO_ADD, std::size_t... SEQ>
        constexpr auto shift_seq_minus(std::index_sequence<SEQ...> &&)
            -> std::index_sequence<TO_ADD - SEQ ...>;
    }

    template <std::size_t MIN, std::size_t MAX>
    using MakeIndexRange = decltype(detail::shift_seq_plus<MIN>(std::declval<std::make_index_sequence<MAX-MIN>>()));

    template <std::size_t MAX, std::size_t MIN>
    using MakeReverseIndexRange = decltype(detail::shift_seq_minus<MAX - 1U>(std::declval<std::make_index_sequence<MAX-MIN>>()));
}

#endif // METAXXA_INDEXRANGE_H


#ifndef METAXXA_ALGORITHM_H
#define METAXXA_ALGORITHM_H


#ifndef METAXXA_ALGORITHM_ONLYINDICES_H
#define METAXXA_ALGORITHM_ONLYINDICES_H


namespace metaxxa
{
    template 
    <
        template <typename...> typename Template, 
        typename TupleT, 
        std::size_t... INDICES
    >
    using OnlyIndices = Template<std::tuple_element_t<INDICES, TupleT>...>;
}

#endif // METAXXA_ALGORITHM_ONLYINDICES_H

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
            -> OnlyIndices<Template, TupleT, INDICES...>;
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


#ifndef METAXXA_ALGORITHM_TAKERANGE_H
#define METAXXA_ALGORITHM_TAKERANGE_H



namespace metaxxa
{
    template
    <
        template <typename...> typename Template,
        typename TupleT,
        std::size_t FROM_I,
        std::size_t TO_I
    >
    using TakeRange = SeqFilter
    <
        Template,
        TupleT,
        MakeIndexRange<FROM_I, TO_I>
    >;
}

#endif // METAXXA_ALGORITHM_TAKERANGE_H

#ifndef METAXXA_ALGORITHM_TAKEFIRST_H
#define METAXXA_ALGORITHM_TAKEFIRST_H


namespace metaxxa
{
    template
    <
        template <typename...> typename Template,
        typename TupleT,
        std::size_t N
    >
    using TakeFirst = TakeRange
    <
        Template,
        TupleT,
        0, N
    >;
}

#endif // METAXXA_ALGORITHM_TAKEFIRST_H

#ifndef METAXXA_ALGORITHM_TAKELAST_H
#define METAXXA_ALGORITHM_TAKELAST_H


namespace metaxxa
{
    template
    <
        template <typename...> typename Template,
        typename TupleT,
        std::size_t N
    >
    using TakeLast = TakeRange
    <
        Template,
        TupleT,
        std::tuple_size_v<TupleT> - N, std::tuple_size_v<TupleT>
    >;
}

#endif // METAXXA_ALGORITHM_TAKELAST_H

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
    using SkipFirst = TakeRange
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
    using SkipLast = TakeRange
    <
        Template,
        TupleT,
        0, std::tuple_size_v<TupleT> - N
    >;
}

#endif // METAXXA_ALGORITHM_SKIPLAST_H

#ifndef METAXXA_ALGORITHM_FIND_H
#define METAXXA_ALGORITHM_FIND_H


#define EXPAND(...) __VA_ARGS__

#define DEFINE_FIND_ALGORITHM(Variant, ...) \
    template                                                    \
    <                                                           \
        typename TupleT,                                        \
        EXPAND(FUNCTOR_TYPE),                                   \
        std::size_t N   = 0,                                    \
        bool PREV_FOUND = false,                                \
        bool ENOUGH     = (N >= std::tuple_size_v<TupleT>)      \
    >                                                           \
    struct Variant##Find : Variant##Find                        \
    <                                                           \
        TupleT,                                                 \
        Functor,                                                \
        N + 1,                                                  \
        Functor<__VA_ARGS__>::value(),                          \
        N + 1 >= std::tuple_size_v<TupleT>                      \
    >                                                           \
    {};                                                         \
                                                                \
    template                                                    \
    <                                                           \
        typename TupleT,                                        \
        EXPAND(FUNCTOR_TYPE),                                   \
        std::size_t N,                                          \
        bool ENOUGH                                             \
    >                                                           \
    struct Variant##Find                                        \
    <                                                           \
        TupleT,                                                 \
        Functor,                                                \
        N,                                                      \
        true,                                                   \
        ENOUGH                                                  \
    >                                                           \
    {                                                           \
        static constexpr bool FOUND = true;                     \
        static constexpr std::size_t INDEX = N - 1;             \
                                                                \
        using Type = std::tuple_element_t<INDEX, TupleT>;       \
                                                                \
        template <typename T>                                   \
        using TypeOr = Type;                                    \
    };                                                          \
                                                                \
    template                                                    \
    <                                                           \
        typename TupleT,                                        \
        EXPAND(FUNCTOR_TYPE),                                   \
        std::size_t N                                           \
    >                                                           \
    struct Variant##Find                                        \
    <                                                           \
        TupleT,                                                 \
        Functor,                                                \
        N,                                                      \
        false,                                                  \
        true                                                    \
    >                                                           \
    {                                                           \
        static constexpr bool FOUND = false;                    \
                                                                \
        template <typename T>                                   \
        using TypeOr = T;                                       \
    }                                                          

namespace metaxxa
{
    namespace detail
    {
        #define FUNCTOR_TYPE template <typename T> typename Functor

        DEFINE_FIND_ALGORITHM(, std::tuple_element_t<N, TupleT>);

        #undef FUNCTOR_TYPE
        #define FUNCTOR_TYPE template <typename T, std::size_t INDEX> typename Functor

        DEFINE_FIND_ALGORITHM(Index, std::tuple_element_t<N, TupleT>, N);

        #undef FUNCTOR_TYPE
        #define FUNCTOR_TYPE template <typename T, std::size_t INDEX, typename SrcTuple> typename Functor

        DEFINE_FIND_ALGORITHM(Overall, std::tuple_element_t<N, TupleT>, N, TupleT);
    }

    template 
    <
        typename TupleT,
        template <typename T> typename Functor
    >
    using Find = detail::Find<TupleT, Functor>;

    template 
    <
        typename TupleT,
        template <typename T, std::size_t INDEX> typename Functor
    >
    using IndexFind = detail::IndexFind<TupleT, Functor>;

    template 
    <
        typename TupleT,
        template <typename T, std::size_t INDEX, typename SrcTuple> typename Functor
    >
    using OverallFind = detail::OverallFind<TupleT, Functor>;
}

#undef FUNCTOR_TYPE
#undef DEFINE_FIND_ALGORITHM
#undef EXPAND

#endif // METAXXA_ALGORITHM_FIND_H

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
    using SkipRange = Concat
    <
        Template,
        TakeFirst<TypeList, TupleT, FROM_I>,
        TakeLast<TypeList, TupleT, std::tuple_size_v<TupleT> - TO_I>
    >;
}

#endif // METAXXA_ALGORITHM_SKIPRANGE_H

#ifndef METAXXA_ALGORITHM_FILTER_H
#define METAXXA_ALGORITHM_FILTER_H



namespace metaxxa
{
    namespace detail
    {
        template 
        <
            typename TupleT,
            std::size_t INDEX,
            bool FunctorResult
        >
        struct ResolveFilterType
        {
            using Type = TemplateContainer<std::tuple_element_t<INDEX, TupleT>>;
        };

        template 
        <
            typename TupleT,
            std::size_t INDEX
        >
        struct ResolveFilterType<TupleT, INDEX, false>
        {
            using Type = TemplateContainer<>;
        };

        template 
        <
            template <typename...> typename Template,
            typename TupleT,
            template <typename T> typename Functor,
            std::size_t... INDICES
        >
        constexpr auto filter_types(std::index_sequence<INDICES...>)
            -> Concat
                <
                    Template, 
                    typename ResolveFilterType
                    <
                        TupleT, 
                        INDICES, 
                        Functor<std::tuple_element_t<INDICES, TupleT>>::value()
                    >::Type...
                >;

        template 
        <
            template <typename...> typename Template,
            typename TupleT,
            template <typename T, std::size_t INDEX> typename Functor,
            std::size_t... INDICES
        >
        constexpr auto index_filter_types(std::index_sequence<INDICES...>)
            -> Concat
                <
                    Template, 
                    typename ResolveFilterType
                    <
                        TupleT, 
                        INDICES, 
                        Functor<std::tuple_element_t<INDICES, TupleT>, INDICES>::value()
                    >::Type...
                >;

        template 
        <
            template <typename...> typename Template,
            typename TupleT,
            template <typename T, std::size_t INDEX, typename SrcTuple> typename Functor,
            std::size_t... INDICES
        >
        constexpr auto overall_filter_types(std::index_sequence<INDICES...>)
            -> Concat
                <
                    Template, 
                    typename ResolveFilterType
                    <
                        TupleT, 
                        INDICES, 
                        Functor<std::tuple_element_t<INDICES, TupleT>, INDICES, TupleT>::value()
                    >::Type...
                >;
    }

    template 
    <
        template <typename...> typename Template,
        typename TupleT,
        template <typename T> typename Functor
    >
    using Filter = decltype
    (
        detail::filter_types
        <
            Template, 
            TupleT, 
            Functor
        >(std::make_index_sequence<std::tuple_size_v<TupleT>>())
    );

    template 
    <
        template <typename...> typename Template,
        typename TupleT,
        template <typename T, std::size_t INDEX> typename Functor
    >
    using IndexFilter = decltype
    (
        detail::index_filter_types
        <
            Template, 
            TupleT, 
            Functor
        >(std::make_index_sequence<std::tuple_size_v<TupleT>>())
    );

    template 
    <
        template <typename...> typename Template,
        typename TupleT,
        template <typename T, std::size_t INDEX, typename SrcTuple> typename Functor
    >
    using OverallFilter = decltype
    (
        detail::overall_filter_types
        <
            Template, 
            TupleT, 
            Functor
        >(std::make_index_sequence<std::tuple_size_v<TupleT>>())
    );
}

#endif // METAXXA_ALGORITHM_FILTER_H


#ifndef METAXXA_ALGORITHM_INVOKEFUNCTIONS_INC
#define METAXXA_ALGORITHM_INVOKEFUNCTIONS_INC


#ifndef METAXXA_ALGORITHM_INVOKEFUNCTIONS_H
#define METAXXA_ALGORITHM_INVOKEFUNCTIONS_H



#ifndef METAXXA_DEF_H
#define METAXXA_DEF_H

#ifdef _MSC_VER
    #define metaxxa_inline __forceinline
#elif defined(__GNUC__)
    #define metaxxa_inline inline __attribute__((__always_inline__))
#elif defined(__CLANG__)
    #if __has_attribute(__always_inline__)
        #define metaxxa_inline inline __attribute__((__always_inline__))
    #else
        #define metaxxa_inline inline
    #endif
#else
    #define metaxxa_inline inline
#endif

#endif // METAXXA_DEF_H

namespace metaxxa
{
    namespace detail
    {
        template
        <
            typename Function,
            bool IS_INVOCABLE,
            typename... Args
        >
        struct Invoker
        {
            metaxxa_inline static void invoke(Function &function, Args&&... args);

            metaxxa_inline static void invoke(const Function &function, Args&&... args);
        };

        template
        <
            typename Function,
            typename... Args
        >
        struct Invoker<Function, false, Args...>
        {
            metaxxa_inline static void invoke(const Function &, Args&&...);
        };

        template 
        <
            typename Tuple, 
            typename... Args
        >
        struct FunctionsInvoker
        {
            template <std::size_t... INDICES>
            metaxxa_inline static void invoke(std::index_sequence<INDICES...>, Tuple &, Args&&...);

            template <std::size_t... INDICES>
            metaxxa_inline static void invoke(std::index_sequence<INDICES...>, const Tuple &, Args&&...);
        };
    }

    template <typename Tuple, typename... Args>
    void invoke_functions(Tuple &, Args&&...);

    template <typename Tuple, typename... Args>
    void invoke_functions(const Tuple &, Args&&...);
}

#endif // METAXXA_ALGORITHM_INVOKEFUNCTIONS_H

#ifndef METAXXA_TUPLE_H
#define METAXXA_TUPLE_H



namespace metaxxa
{
    template <typename... Types>
    class Tuple : public TypeTuple<Types...>
    {
    public:
        using TypeTuple = metaxxa::TypeTuple<Types...>;

        Tuple();

        Tuple(Types&&... args);

        Tuple(const Types&... args);

        template <typename TupleT>
        Tuple(const TupleT &);

        Tuple(const Tuple &);

        Tuple(Tuple &&);

        ~Tuple();

        template <typename TupleT>
        Tuple &operator=(const TupleT &);

        Tuple &operator=(const Tuple &);

        Tuple &operator=(Tuple &&);

        template <std::size_t INDEX>
        metaxxa_inline auto &get();

        template <std::size_t INDEX>
        metaxxa_inline const auto &get() const;

        template <typename T>
        metaxxa_inline auto &get(std::size_t index);

        template <typename T>
        metaxxa_inline const auto &get(std::size_t index) const;

        metaxxa_inline void *get(std::size_t index);

        metaxxa_inline const void *get(std::size_t index) const;

    private:
        template <std::size_t... INDICES>
        metaxxa_inline void construct(std::index_sequence<INDICES...>);

        template <std::size_t... INDICES>
        metaxxa_inline void construct(Types&&... args, std::index_sequence<INDICES...>);

        template <std::size_t... INDICES>
        metaxxa_inline void construct(const Types&... args, std::index_sequence<INDICES...>);

        template <typename OtherTuple, std::size_t... INDICES>
        metaxxa_inline void construct(const OtherTuple &other, std::index_sequence<INDICES...>);

        template <std::size_t... INDICES>
        metaxxa_inline void deallocate(std::index_sequence<INDICES...>);

        template <std::size_t INDEX, typename T>
        metaxxa_inline void deallocate();

        unsigned char *data;
        std::size_t    offsets[TypeTuple::size()];
    };

    template <typename TupleT>
    using TupleFrom = MoveParameters<Tuple, TupleT>;
}

namespace std
{
    template <std::size_t INDEX, typename... Args>
    class tuple_element<INDEX, metaxxa::Tuple<Args...>>
    {
    public:
        using type = std::tuple_element_t<INDEX, typename metaxxa::Tuple<Args...>::TypeTuple>;
    };

    template <typename... Args>
    class tuple_size<metaxxa::Tuple<Args...>>
    {
    public:
        static constexpr std::size_t value = std::tuple_size_v<typename metaxxa::Tuple<Args...>::TypeTuple>;
    };

    template <std::size_t INDEX, typename... Args>
    auto &get(metaxxa::Tuple<Args...> &);

    template <std::size_t INDEX, typename... Args>
    auto &get(const metaxxa::Tuple<Args...> &);
}

#endif // METAXXA_TUPLE_H

namespace metaxxa
{
    namespace detail
    {
        template
        <
            typename Function,
            bool IS_INVOCABLE,
            typename... Args
        >
        metaxxa_inline void Invoker<Function, IS_INVOCABLE, Args...>::invoke(Function &function, Args&&... args)
        {
            std::invoke(function, std::forward<Args>(args)...);
        }

        template
        <
            typename Function,
            bool IS_INVOCABLE,
            typename... Args
        >
        metaxxa_inline void Invoker<Function, IS_INVOCABLE, Args...>::invoke(const Function &function, Args&&... args)
        {
            std::invoke(function, std::forward<Args>(args)...);
        }

        template
        <
            typename Function,
            typename... Args
        >
        metaxxa_inline void Invoker<Function, false, Args...>::invoke(const Function &, Args&&...)
        {}
        

        template 
        <
            typename Tuple, 
            typename... Args
        >
        template <std::size_t... INDICES>
        metaxxa_inline void FunctionsInvoker<Tuple, Args...>::invoke(std::index_sequence<INDICES...>, Tuple &tuple, Args&&... args)
        {
            (
                Invoker
                <
                    std::tuple_element_t<INDICES, Tuple>, 
                    std::is_invocable_v<std::tuple_element_t<INDICES, Tuple>, Args...>, 
                    Args...
                >::invoke
                (
                    std::get<INDICES>(tuple), 
                    std::forward<Args>(args)...
                ),
                ...
            );
        }

        template 
        <
            typename Tuple, 
            typename... Args
        >
        template <std::size_t... INDICES>
        metaxxa_inline void FunctionsInvoker<Tuple, Args...>::invoke(std::index_sequence<INDICES...>, const Tuple &tuple, Args&&... args)
        {
            (
                Invoker
                <
                    std::tuple_element_t<INDICES, Tuple>, 
                    std::is_invocable_v<std::tuple_element_t<INDICES, Tuple>, Args...>, 
                    Args...
                >::invoke
                (
                    std::get<INDICES>(tuple), 
                    std::forward<Args>(args)...
                ),
                ...
            );
        }
    }

    template <typename Tuple, typename... Args>
    void invoke_functions(Tuple &tuple, Args&&... args)
    {
        detail::FunctionsInvoker<Tuple, Args...>::invoke(std::make_index_sequence<std::tuple_size_v<Tuple>>(), tuple, std::forward<Args>(args)...);
    }

    template <typename Tuple, typename... Args>
    void invoke_functions(const Tuple &tuple, Args&&... args)
    {
        detail::FunctionsInvoker<Tuple, Args...>::invoke(std::make_index_sequence<std::tuple_size_v<Tuple>>(), tuple, std::forward<Args>(args)...);
    }
}

#endif // METAXXA_ALGORITHM_INVOKEFUNCTIONS_INC

#endif // METAXXA_ALGORITHM_H


#ifndef METAXXA_SWITCH_H
#define METAXXA_SWITCH_H


namespace metaxxa
{
    namespace detail
    {
        template <typename T>
        struct IsTrue
        {
            static constexpr bool value() { return T::value(); }
        };

        template <bool RESULT, typename CaseType>
        struct CasePair
        {
            static constexpr bool value() { return RESULT; }

            using Type = CaseType;            
        };

        template 
        <
            typename SwitchListT, 
            typename FindFirstTrue = Find<SwitchListT, IsTrue>, 
            bool IS_FOUND = FindFirstTrue::FOUND
        >
        class CaseImplBase
        {
        protected:
            using SwitchList = SwitchListT;

        public:
            using Type = typename std::tuple_element_t
            <
                FindFirstTrue::INDEX, 
                SwitchList
            >::Type;

            template <typename DefaultT>
            using DefaultType = Type;
        };

        template 
        <
            typename SwitchListT, 
            typename FindFirstTrue
        >
        class CaseImplBase<SwitchListT, FindFirstTrue, false>
        {
        protected:
            using SwitchList = SwitchListT;

        public:
            template <typename DefaultT>
            using DefaultType = DefaultT;
        };

        template 
        <
            typename T, 
            T SRC_CONSTANT, 
            typename OldSwitchList, 
            T CASE_CONSTANT, 
            typename CaseType
        >
        class CaseImpl : public CaseImplBase
        <
            Concat
            <
                TypeList, 
                OldSwitchList, 
                TypeList<CasePair<SRC_CONSTANT == CASE_CONSTANT, CaseType>>
            >
        >
        {
            using Base = CaseImplBase
            <
                Concat
                <
                    TypeList, 
                    OldSwitchList, 
                    TypeList<CasePair<SRC_CONSTANT == CASE_CONSTANT, CaseType>>
                >
            >;

        public:
            template <T NEXT_CONSTANT, typename NextCaseType>
            using Case = CaseImpl
            <
                T, 
                SRC_CONSTANT, 
                typename Base::SwitchList,
                NEXT_CONSTANT, 
                NextCaseType
            >;
        };
    }

    template <typename T, T CONSTANT>
    struct Switch
    {
        template <T CASE_CONSTANT, typename CaseType>
        using Case = detail::CaseImpl
        <
            T,
            CONSTANT,
            TypeList<>,
            CASE_CONSTANT,
            CaseType
        >;
    };
}

#endif // METAXXA_SWITCH_H

#ifndef METAXXA_TYPESWITCH_H
#define METAXXA_TYPESWITCH_H


namespace metaxxa
{
    namespace detail
    {
        template 
        <
            typename T,
            typename OldSwitchList, 
            typename CaseCondition, 
            typename CaseType
        >
        class TypeCaseImpl : public CaseImplBase
        <
            Concat
            <
                TypeList, 
                OldSwitchList, 
                TypeList<CasePair<std::is_same_v<T, CaseCondition>, CaseType>>
            >
        >
        {
            using Base = CaseImplBase
            <
                Concat
                <
                    TypeList, 
                    OldSwitchList, 
                    TypeList<CasePair<std::is_same_v<T, CaseCondition>, CaseType>>
                >
            >;

        public:
            template <typename NextTypeCondition, typename NextCaseType>
            using Case = TypeCaseImpl
            <
                T, 
                typename Base::SwitchList,
                NextTypeCondition, 
                NextCaseType
            >;
        };
    }

    template <typename T>
    struct TypeSwitch
    {
        template <typename CaseCondition, typename CaseType>
        using Case = detail::TypeCaseImpl<T, TypeList<>, CaseCondition, CaseType>;
    };
}

#endif // METAXXA_TYPESWITCH_H


#ifndef METAXXA_MAKEFUNCTIONTYPE_H
#define METAXXA_MAKEFUNCTIONTYPE_H



namespace metaxxa
{
    namespace detail
    {
        template <typename ResultType>
        struct MakeFunctionType
        {
            template <typename... Args>
            using Type = ResultType(Args...);
        };

        template <typename Tuple, std::size_t RETURN_INDEX>
        struct MakeFunctionTypeImpl
        {
            using ResultType = std::tuple_element_t<RETURN_INDEX, Tuple>;
            using ArgsList  = SkipRange<TypeList, Tuple, RETURN_INDEX, RETURN_INDEX + 1>;

            using Type = MoveParameters
            <
                MakeFunctionType<ResultType>::template Type,
                ArgsList
            >;
        };
    }

    template <typename Tuple, std::size_t RETURN_INDEX>
    using MakeFunctionType = typename detail::MakeFunctionTypeImpl<Tuple, RETURN_INDEX>::Type;
}

#endif // METAXXA_MAKEFUNCTIONTYPE_H


#ifndef METAXXA_ENABLEFNIF_H
#define METAXXA_ENABLEFNIF_H


#define ENABLE_T_IF(CONDITION) typename = std::enable_if_t<CONDITION>

#define ENABLE_FN_IF_T(CONDITION) std::enable_if_t<CONDITION> *

#define ENABLE_FN_IF(CONDITION) ENABLE_FN_IF_T(CONDITION) = nullptr

#endif // METAXXA_ENABLEFNIF_H


#ifndef METAXXA_TUPLE_INC
#define METAXXA_TUPLE_INC


namespace metaxxa
{
    namespace detail
    {
        template <typename... Args>
        constexpr std::size_t memory_size()
        {
            return (0 + ... + sizeof(Args));
        }

        template <template <typename...> typename Tuple, typename... Args>
        constexpr std::size_t memory_size(Tuple<Args...> &&)
        {
            return memory_size<Args...>();
        }

        template <typename... Args>
        struct Copyist
        {
            template <typename... ArgsRHS>
            metaxxa_inline static void copy
            (
                unsigned char *dest,
                std::size_t *dest_offsets,

                const unsigned char *src,
                std::size_t *src_offsets,
                ENABLE_FN_IF(sizeof...(Args) == sizeof...(ArgsRHS))
            )
            {
                constexpr std::size_t N = sizeof...(Args);

                for(std::size_t i = 0; i < N; ++i)
            }
        };
    }

    template <typename... Args>
    Tuple<Args...>::Tuple()
    : data(static_cast<unsigned char *>(malloc(detail::memory_size<Args...>())))
    {
        construct(std::make_index_sequence<TypeTuple::size()>());
    }

    template <typename... Args>
    Tuple<Args...>::Tuple(Args&&... args)
    : data(static_cast<unsigned char *>(malloc(detail::memory_size<Args...>())))
    {
        construct(std::forward<Args>(args)..., std::make_index_sequence<TypeTuple::size()>());
    }

    template <typename... Args>
    Tuple<Args...>::Tuple(const Args&... args)
    : data(static_cast<unsigned char *>(malloc(detail::memory_size<Args...>())))
    {
        construct(args..., std::make_index_sequence<TypeTuple::size()>());
    }

    template <typename... Args>
    template <typename TupleT>
    Tuple<Args...>::Tuple(const TupleT &other)
    : data(static_cast<unsigned char *>(malloc(detail::memory_size<Args...>())))
    {
        construct(other, std::make_index_sequence<std::tuple_size_v<TupleT>>());
    }

    template <typename... Args>
    Tuple<Args...>::Tuple(const Tuple &other)
    : data(static_cast<unsigned char *>(malloc(detail::memory_size<Args...>())))
    {
        construct(other, std::make_index_sequence<TypeTuple::size()>());
    }

    template <typename... Args>
    Tuple<Args...>::Tuple(Tuple &&other)
    : data(other.data)
    {
        other.data = nullptr;
        for(auto i = 0u; i < sizeof...(Args); ++i)
            offsets[i] = other.offsets[i];
    }

    template <typename... Args>
    Tuple<Args...>::~Tuple()
    {
        deallocate(MakeReverseIndexRange<TypeTuple::size(), 0>());
    }

    template <typename... Args>
    template <typename TupleT>
    Tuple<Args...> &Tuple<Args...>::operator=(const TupleT &rhs)
    {
        return *this = std::move(Tuple(rhs));
    }

    template <typename... Args>
    Tuple<Args...> &Tuple<Args...>::operator=(const Tuple &rhs)
    {
        if(this != &rhs)
            *this = std::move(Tuple(rhs));

        return *this;
    }

    template <typename... Args>
    Tuple<Args...> &Tuple<Args...>::operator=(Tuple &&rhs)
    {
        data = rhs.data;
        rhs.data = nullptr;
        return *this;
    }

    template <typename... Args>
    metaxxa_inline void *Tuple<Args...>::get(std::size_t index)
    {
        return static_cast<void *>(data + offsets[index]);
    }

    template <typename... Args>
    metaxxa_inline const void *Tuple<Args...>::get(std::size_t index) const
    {
        return const_cast<Tuple<Args...>*>(this)->get(index);
    }

    template <typename... Args>
    template <typename T>
    metaxxa_inline auto &Tuple<Args...>::get(std::size_t index)
    {
        return *static_cast<T*>(get(index));
    }

    template <typename... Args>
    template <typename T>
    metaxxa_inline const auto &Tuple<Args...>::get(std::size_t index) const
    {
        return const_cast<Tuple<Args...>*>(this)->template get<T>(index);
    }

    template <typename... Args>
    template <std::size_t INDEX>
    metaxxa_inline auto &Tuple<Args...>::get()
    {
        return get<typename TypeTuple::template Get<INDEX>>(INDEX);
    }

    template <typename... Args>
    template <std::size_t INDEX>
    metaxxa_inline const auto &Tuple<Args...>::get() const
    {
        return const_cast<Tuple<Args...>*>(this)->template get<INDEX>();
    }

    template <typename... Args>
    template <std::size_t... INDICES>
    metaxxa_inline void Tuple<Args...>::construct(std::index_sequence<INDICES...>)
    {
        ((void)(offsets[INDICES] = detail::memory_size(TakeFirst<TypeList, TypeTuple, INDICES>())), ...);

        if(data)
            ((void)(new (get(INDICES)) typename TypeTuple::template Get<INDICES>()), ...);
    }

    template <typename... Args>
    template <std::size_t... INDICES>
    metaxxa_inline void Tuple<Args...>::construct(Args&&... args, std::index_sequence<INDICES...>)
    {
        ((void)(offsets[INDICES] = detail::memory_size(TakeFirst<TypeList, TypeTuple, INDICES>())), ...);

        if(data)
            ((void)(new (get(INDICES)) typename TypeTuple::template Get<INDICES>(std::forward<Args>(args))), ...);
    }

    template <typename... Args>
    template <std::size_t... INDICES>
    metaxxa_inline void Tuple<Args...>::construct(const Args&... args, std::index_sequence<INDICES...>)
    {
        ((void)(offsets[INDICES] = detail::memory_size(TakeFirst<TypeList, TypeTuple, INDICES>())), ...);

        if(data)
            ((void)(new (get(INDICES)) typename TypeTuple::template Get<INDICES>(args)), ...);
    }

    template <typename... Args>
    template <typename OtherTuple, std::size_t... INDICES>
    metaxxa_inline void Tuple<Args...>::construct(const OtherTuple &other, std::index_sequence<INDICES...>)
    {
        ((void)(offsets[INDICES] = detail::memory_size(TakeFirst<TypeList, TypeTuple, INDICES>())), ...);

        if(data)
            ((void)(new (get(INDICES)) typename TypeTuple::template Get<INDICES>(std::get<INDICES>(other))), ...);
    }

    template <typename... Args>
    template <std::size_t... INDICES>
    metaxxa_inline void Tuple<Args...>::deallocate(std::index_sequence<INDICES...>)
    {
        if(data)
        {
            (deallocate<INDICES, typename TypeTuple::template Get<INDICES>>(), ...);
            ::free(data);
        }
    }

    template <typename... Args>
    template <std::size_t INDEX, typename T>
    metaxxa_inline void Tuple<Args...>::deallocate()
    {
        get<INDEX>().~T();
    }
}

namespace std
{
    template <std::size_t INDEX, typename... Args>
    auto &get(metaxxa::Tuple<Args...> &tuple)
    {
        return tuple.template get<INDEX>();
    }

    template <std::size_t INDEX, typename... Args>
    auto &get(const metaxxa::Tuple<Args...> &tuple)
    {
        return tuple.template get<INDEX>();
    }
}

#endif // METAXXA_TUPLE_INC


#endif // METAXXA_HPP

#ifdef _MSC_VER
    #define mafox_inline __forceinline
#elif defined(__GNUC__)
    #define mafox_inline inline __attribute__((__always_inline__))
#elif defined(__CLANG__)
    #if __has_attribute(__always_inline__)
        #define mafox_inline inline __attribute__((__always_inline__))
    #else
        #define mafox_inline inline
    #endif
#else
    #define mafox_inline inline
#endif

#define MAFOX_EXPAND(...) __VA_ARGS__

#define MAFOX_NCPTR(PTR) const_cast<MAFOX_EXPAND(MAFOX_SELF)*>(PTR)

#define MAFOX_DEFAULT_EPS 0.001

namespace mafox
{
    using Byte = unsigned char;
}

#endif // MAFOX_DETAIL_DEF_H

#ifndef MAFOX_GMATRIX_INC
#define MAFOX_GMATRIX_INC


#ifndef MAFOX_GMATRIX_H
#define MAFOX_GMATRIX_H




#ifndef MAFOX_ORIENTATION_H
#define MAFOX_ORIENTATION_H

namespace mafox
{
    enum Orientation
    {
        HORIZONTAL,
        VERTICAL
    };
}

#endif // MAFOX_ORIENTATION_H

namespace mafox
{
    enum MatrixOrder
    {
        ROW_MAJOR,
        COLUMN_MAJOR
    };

    template 
    <
        typename T,
        typename Allocator = std::allocator<T>
    >
    struct GMatrix
    {
        GMatrix(std::size_t rows, std::size_t cols, T *data, MatrixOrder);

        GMatrix
        (
            std::size_t rows, 
            std::size_t cols, 
            metaxxa::TypeOrRef<const T> initial_value = T(), 
            MatrixOrder = ROW_MAJOR
        );

        GMatrix(const GMatrix &);

        GMatrix(GMatrix &&);

        ~GMatrix();

        GMatrix &operator=(const GMatrix<T> &);

        GMatrix &operator=(GMatrix<T> &&);

        mafox_inline std::size_t rows() const;

        mafox_inline std::size_t cols() const;

        mafox_inline void resize(std::size_t rows, std::size_t cols);

        mafox_inline T *data();

        mafox_inline const T *data() const;

        mafox_inline metaxxa::TypeOrRef<T> at(std::size_t i, std::size_t j);

        mafox_inline metaxxa::TypeOrRef<const T> at(std::size_t i, std::size_t j) const;

        mafox_inline void set_at(std::size_t i, std::size_t j, metaxxa::TypeOrRef<T> value);

        std::size_t _rows, _cols;
        T *t_data;
        MatrixOrder order;
    };
}

#endif // MAFOX_DETAIL_GMATRIX_H


#ifdef _MSC_VER
#   pragma warning(push)
#   pragma warning(disable: 4003)
#endif // _MSC_VER

#define MAFOX_SELF GMatrix<T, Allocator>

#define MAFOX_GMATRIX(ReturnType) \
    template <typename T, typename Allocator> \
    ReturnType GMatrix<T, Allocator>

#define INLINE_MAFOX_GMATRIX(ReturnType) \
    template <typename T, typename Allocator> \
    MAFOX_EXPAND(mafox_inline) ReturnType GMatrix<T, Allocator>

namespace mafox
{
    namespace detail
    {
        template <typename T, typename Allocator>
        T *allocate_matrix_data(std::size_t rows, std::size_t cols)
        {
            // TODO: use allocator
           return new T[rows*cols];
        }

        template <typename T, typename Allocator>
        T *reallocate_matrix_data(T *, std::size_t rows, std::size_t cols)
        {
            // TODO: use allocator
           return allocate_matrix_data<T, Allocator>(rows, cols);
        }

        template <typename T, typename Allocator>
        void deallocate_matrix_data(T *addr, std::size_t, std::size_t)
        {
            // TODO: use allocator
            delete []addr;
        }
    }

    MAFOX_GMATRIX()::GMatrix(std::size_t rows, std::size_t cols, T *data, MatrixOrder order)
    : _rows(rows), _cols(cols), t_data(data), order(order)
    {
        assert(rows != 0 && cols != 0);
    }

    MAFOX_GMATRIX()::GMatrix
    (
        std::size_t rows, 
        std::size_t cols, 
        metaxxa::TypeOrRef<const T> initial_value,
        MatrixOrder order
    ): 
    _rows(rows), 
    _cols(cols), 
    t_data(detail::allocate_matrix_data<T, Allocator>(rows, cols)), 
    order(order)
    {
        assert(rows != 0 && cols != 0);

        // TODO: Parallel
        for(std::size_t i = 0, j = 0; i < rows; ++i)
            for(j = 0; j < cols; ++j)
            {
                if(order == ROW_MAJOR)
                    t_data[i*cols + j] = initial_value;
                else
                    t_data[j*cols + i] = initial_value;
            }
    }

    MAFOX_GMATRIX()::GMatrix(const GMatrix &other)
    : _rows(other._rows),
    _cols(other._cols),
    t_data(detail::allocate_matrix_data<T, Allocator>(_rows, _cols)),
    order(other.order)
    {
        const std::size_t size = _rows*_cols*sizeof(T);

        std::memcpy
        (
            static_cast<void *>(t_data),
            static_cast<const void *>(other.t_data),
            size
        );
    }

    MAFOX_GMATRIX()::GMatrix(GMatrix &&other)
    : _rows(other._rows),
    _cols(other._cols),
    t_data(other.t_data),
    order(other.order)
    {
        other.t_data = nullptr;
    }

    MAFOX_GMATRIX()::~GMatrix()
    {
        detail::deallocate_matrix_data<T, Allocator>(t_data, _rows, _cols);
    }

    MAFOX_GMATRIX(MAFOX_SELF &)::operator=(const GMatrix<T> &rhs)
    {
        if(this != &rhs)
        {
            _rows   = rhs._rows;
            _cols   = rhs._cols;
            order  = rhs.order;
            t_data = detail::reallocate_matrix_data<T, Allocator>(t_data, _rows, _cols);

            const std::size_t size = _rows*_cols*sizeof(T);

            memcpy
            (
                static_cast<void *>(t_data),
                static_cast<const void *>(rhs.t_data),
                size
            );
        }

        return *this;
    }

    MAFOX_GMATRIX(MAFOX_SELF &)::operator=(GMatrix<T> &&rhs)
    {
        _rows   = rhs._rows;
        _cols   = rhs._cols;
        order  = rhs.order;
        t_data = rhs.t_data;
        return *this;
    }

    INLINE_MAFOX_GMATRIX(std::size_t)::rows() const
    {
        return _rows;
    }

    INLINE_MAFOX_GMATRIX(std::size_t)::cols() const
    {
        return _cols;
    }

    INLINE_MAFOX_GMATRIX(void)::resize(std::size_t rows, std::size_t cols)
    {
        this->_rows = rows;
        this->_cols = cols;
        t_data = detail::reallocate_matrix_data<T, Allocator>(t_data, _rows, _cols);
    }

    INLINE_MAFOX_GMATRIX(T *)::data()
    {
        return t_data;
    }

    INLINE_MAFOX_GMATRIX(const T *)::data() const
    {
        return MAFOX_NCPTR(this)->data();
    }

    INLINE_MAFOX_GMATRIX(metaxxa::TypeOrRef<T>)::at(std::size_t i, std::size_t j)
    {
        if(order == ROW_MAJOR)
            return t_data[i*_cols + j];
        else
            return t_data[j*_cols + i];
    }

    INLINE_MAFOX_GMATRIX(metaxxa::TypeOrRef<const T>)::at(std::size_t i, std::size_t j) const
    {
        return MAFOX_NCPTR(this)->at(i, j);
    }

    INLINE_MAFOX_GMATRIX(void)::set_at(std::size_t i, std::size_t j, metaxxa::TypeOrRef<T> value)
    {
        if(order == ROW_MAJOR)
            t_data[i*_cols + j] = value;
        else
            t_data[j*_cols + i] = value;
    }
}

#ifdef _MSC_VER
#   pragma warning(pop)
#endif // _MSC_VER

#undef INLINE_MAFOX_GMATRIX
#undef MAFOX_GMATRIX
#undef MAFOX_SELF

#endif // MAFOX_GMATRIX_INC

#ifndef MAFOX_LEGENDRE_INC
#define MAFOX_LEGENDRE_INC


#ifndef MAFOX_LEGENDRE_H
#define MAFOX_LEGENDRE_H



#define ENABLE_IF_INT_POWER ENABLE_FN_IF(std::is_integral_v<IntT>)

namespace mafox
{
    template <typename T, typename IntT>
    mafox_inline auto legendre_polynomial
    (
        metaxxa::TypeOrRef<const T> x,
        IntT power, 
        ENABLE_IF_INT_POWER
    );

    // Returns std::pair of P_n(x) P_{n-1}(x)
    template <typename T, typename IntT>
    auto legendre_polynomial_pair
    (
        metaxxa::TypeOrRef<const T> x,
        IntT power, 
        ENABLE_IF_INT_POWER
    );

    // LP means Legendre Polynomial
    // In LaTeX:
    //      $$ CurrentLP is P_n(x)      $$
    //      $$ PreviousLP is P_{n-1}(x) $$
    template 
    <
        typename T, 
        typename IntT, 
        typename CurrentLP, 
        typename PreviousLP
    >
    mafox_inline auto legendre_polynomial_next
    (
        metaxxa::TypeOrRef<const T> x,
        IntT power,
        metaxxa::TypeOrRef<const CurrentLP>,
        metaxxa::TypeOrRef<const PreviousLP>,
        ENABLE_IF_INT_POWER
    );

    // LP means Legendre Polynomial
    // In LaTeX:
    //      $$ CurrentLP is P_n(x)      $$
    //      $$ PreviousLP is P_{n-1}(x) $$
    template <typename T, typename IntT, typename CurrentLP, typename PreviousLP>
    mafox_inline auto legendre_polynomial_derivative
    (
        metaxxa::TypeOrRef<const T> x,
        IntT power,
        metaxxa::TypeOrRef<const CurrentLP>,
        metaxxa::TypeOrRef<const PreviousLP>,
        ENABLE_IF_INT_POWER
    );

    template 
    <
        typename T, 
        typename IntT, 
        typename RootsContainer,
        typename Eps = T
    >
    void legendre_polynomial_roots
    (
        IntT power,
        RootsContainer &,
        metaxxa::TypeOrRef<const Eps> eps = MAFOX_DEFAULT_EPS
    );

    template 
    <
        typename T, 
        typename IntT, 
        typename RootsContainer,
        typename Eps = T
    >
    RootsContainer legendre_polynomial_roots
    (
        IntT power,
        metaxxa::TypeOrRef<const Eps> eps = MAFOX_DEFAULT_EPS
    );

    template 
    <
        typename T = double, 
        typename IntT = int
    >
    class LegendrePolynomial
    {
    public:
        mafox_inline LegendrePolynomial();

        mafox_inline LegendrePolynomial(IntT power);

        mafox_inline LegendrePolynomial &power(IntT p);

        mafox_inline IntT power() const;

        mafox_inline LegendrePolynomial &next_power();

        mafox_inline auto operator()(metaxxa::TypeOrRef<const T> x) const;

        mafox_inline auto pair(metaxxa::TypeOrRef<const T> x) const;

        mafox_inline auto derivative(metaxxa::TypeOrRef<const T> x) const;

        template <typename RootsContainer = std::vector<T>>
        mafox_inline void roots(RootsContainer &roots, metaxxa::TypeOrRef<const T> eps = MAFOX_DEFAULT_EPS);

        template <typename RootsContainer = std::vector<T>>
        mafox_inline RootsContainer roots(metaxxa::TypeOrRef<const T> eps = MAFOX_DEFAULT_EPS);

    private:
        IntT _power;
    };
}

#undef ENABLE_IF_INT_POWER

#endif // MAFOX_LEGENDRE_H


#define _USE_MATH_DEFINES 
#include <math.h>
#include <cmath>
#include <functional>

#ifdef _MSC_VER
#   pragma warning(push)
#   pragma warning(disable: 4003)
#endif // _MSC_VER

#define ENABLE_IF_INT_POWER ENABLE_FN_IF_T(std::is_integral_v<IntT>)

#define MAFOX_SELF LegendrePolynomial<T, IntT>
    
#define MAFOX_LP(ReturnType) \
    template <typename T, typename IntT> \
    ReturnType MAFOX_EXPAND(MAFOX_SELF)

#define INLINE_MAFOX_LP(ReturnType) \
    template <typename T, typename IntT> \
    MAFOX_EXPAND(mafox_inline) ReturnType MAFOX_EXPAND(MAFOX_SELF)

namespace mafox
{
    template <typename T, typename IntT>
    mafox_inline auto legendre_polynomial
    (
        metaxxa::TypeOrRef<const T> x,
        IntT power, 
        ENABLE_IF_INT_POWER
    )
    {
        return legendre_polynomial_pair<T, IntT>(x, power).first;
    }

    template <typename T, typename IntT>
    auto legendre_polynomial_pair
    (
        metaxxa::TypeOrRef<const T> x,
        IntT power,
        ENABLE_IF_INT_POWER
    )
    {
        assert(power >= 0);
        assert(-1.0 < x && x < 1.0);

        if(power == 0)
            return std::pair<T, T>(T(1.0), T(std::nan("Result of Legendre polynomial of power -1")));
        else if(power == 1)
            return std::pair<T, T>(x, T(1.0));

        std::pair<T, T> pair(x /*= P_1(x)*/, T(1.0) /*= P_0(x)*/);

        // i <= power: for pl_n1 == P_power at end of cycle
        for(IntT i = 1; i < power; ++i)
        {
            std::swap(pair.first, pair.second);
            pair.first = legendre_polynomial_next<T, IntT, T, T>(x, i, pair.second, pair.first);
        }

        return pair;
    }

    template <typename T, typename IntT, typename CurrentLP = T, typename PreviousLP = T>
    mafox_inline auto legendre_polynomial_next
    (
        metaxxa::TypeOrRef<const T> x,
        IntT power,
        metaxxa::TypeOrRef<const CurrentLP> cur_lp,
        metaxxa::TypeOrRef<const PreviousLP> prev_lp,
        ENABLE_IF_INT_POWER
    )
    {
        assert(power >= 0);

        return ((2*power + 1) * x * cur_lp - power * prev_lp) / (power + 1);
    }

    template <typename T, typename IntT, typename CurrentLP = T, typename PreviousLP = T>
    mafox_inline auto legendre_polynomial_derivative
    (
        metaxxa::TypeOrRef<const T> x,
        IntT power,
        metaxxa::TypeOrRef<const CurrentLP> cur_lp,
        metaxxa::TypeOrRef<const PreviousLP> prev_lp,
        ENABLE_IF_INT_POWER
    )
    {
        assert(power >= 0);

        if(power == 0)
            return T(0);

        return power*(prev_lp - x * cur_lp) / (1 - x*x);
    }

    template <typename T, typename IntT, typename RootsContainer, typename Eps>
    void legendre_polynomial_roots
    (
        IntT power,
        RootsContainer &roots,
        metaxxa::TypeOrRef<const Eps> eps
    )
    {
        if(power == 0)
            return;

        roots.resize(power);

        if(power == 1)
        {
            roots[0] = T(0.0);
            return;
        }

        IntT root_i = 0;
        auto div = std::div(power, 2);
        IntT end_i = static_cast<IntT>(div.quot);
        
        auto ceil_q = div.quot;
        IntT neg_root_offset = 1;

        if(div.rem != 0)
        {
            roots[div.quot] = T(0.0);
            
            ++ceil_q;
            --neg_root_offset;
            ++root_i;
            ++end_i;
        }

        T guess(0.0), root(0.0);
        T theta(0.0);
        T p_n(0.0), p_n1(0.0), dp_n(0.0);

        IntT power3 = 8*power*power*power;
        auto k1 = 1.0 - (power - 1.0)/power3;
        auto k2 = 1.0/(48*power3*power);
        auto theta_dem = 4*power + 2;

        for(; root_i < end_i; ++root_i)
        {
            theta = M_PI*(4*(ceil_q - root_i) - 1)/theta_dem;
            auto sine = sin(theta);
            sine *= sine;

            guess = (k1 - k2*(39.0 - 28.0/sine))*cos(theta);
            std::tie(p_n, p_n1) = legendre_polynomial_pair<T, IntT>(guess, power);
            dp_n = legendre_polynomial_derivative<T, IntT, T, T>(guess, power, p_n, p_n1);

            root = guess - p_n / dp_n;
            while(std::abs(root - guess) >= eps)
            {
                std::tie(p_n, p_n1) = legendre_polynomial_pair<T, IntT>(root, power);
                dp_n = legendre_polynomial_derivative<T, IntT, T, T>(root, power, p_n, p_n1);

                guess = root;
                root = guess - p_n / dp_n;
            }

            roots[div.quot + root_i] = root;
            roots[div.quot - root_i - neg_root_offset] = -root;
        }
    }

    template 
    <
        typename T, 
        typename IntT, 
        typename RootsContainer,
        typename Eps
    >
    RootsContainer legendre_polynomial_roots
    (
        IntT power,
        metaxxa::TypeOrRef<const Eps> eps
    )
    {
        RootsContainer container;

        legendre_polynomial_roots<T, IntT, RootsContainer, Eps>(power, container, eps);

        return container;
    }


    INLINE_MAFOX_LP()::LegendrePolynomial()
    : LegendrePolynomial(0)
    {}

    INLINE_MAFOX_LP()::LegendrePolynomial(IntT power)
    : _power(power)
    {}

    INLINE_MAFOX_LP(MAFOX_SELF &)::power(IntT p)
    {
        _power = p;
        return *this;
    }

    INLINE_MAFOX_LP(IntT)::power() const
    {
        return _power;
    }

    INLINE_MAFOX_LP(MAFOX_SELF &)::next_power()
    {
        return power(_power + 1);
    }

    INLINE_MAFOX_LP(auto)::operator()(metaxxa::TypeOrRef<const T> x) const
    {
        return legendre_polynomial<T, IntT>(x, _power);
    }

    INLINE_MAFOX_LP(auto)::pair(metaxxa::TypeOrRef<const T> x) const
    {
        return legendre_polynomial_pair<T, IntT>(x, _power);
    }

    INLINE_MAFOX_LP(auto)::derivative(metaxxa::TypeOrRef<const T> x) const
    {
        if(_power == 0)
            return T(0);

        auto [p_n, p_n1] = pair(x);

        return legendre_polynomial_derivative<T, IntT, decltype(p_n), decltype(p_n1)>
        (
            x,
            _power,
            p_n,
            p_n1
        );
    }

    template <typename T, typename IntT>
    template <typename RootsContainer>
    mafox_inline void MAFOX_SELF::roots(RootsContainer &roots, metaxxa::TypeOrRef<const T> eps)
    {
        return legendre_polynomial_roots<T, IntT, RootsContainer>(_power, roots, eps);
    }

    template <typename T, typename IntT>
    template <typename RootsContainer>
    mafox_inline RootsContainer MAFOX_SELF::roots(metaxxa::TypeOrRef<const T> eps)
    {
        return legendre_polynomial_roots<T, IntT, RootsContainer>(_power, eps);
    }
}

#ifdef _MSC_VER
#   pragma warning(pop)
#endif // _MSC_VER

#undef INLINE_MAFOX_LP
#undef MAFOX_LP
#undef MAFOX_SELF

#undef ENABLE_IF_INT_POWER

#endif // MAFOX_LEGENDRE_INC

#ifndef MAFOX_TABLE_INC
#define MAFOX_TABLE_INC


#ifndef MAFOX_TABLE_H
#define MAFOX_TABLE_H


namespace mafox
{
    namespace detail
    {
        template <typename... Types>
        using TupleType = std::tuple<Types...>;

        template <typename>
        using ReplaceWithSizeT = std::size_t;
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

#ifndef MAFOX_GRIDFUNCTION_INC
#define MAFOX_GRIDFUNCTION_INC


#ifndef MAFOX_GRIDFUNCTION_H
#define MAFOX_GRIDFUNCTION_H



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
            return ValueCatter<Value, Args...>::cat(std::make_index_sequence<args.size()>(), value, args);
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

#ifndef MAFOX_INTEGRALSOLVER_INC
#define MAFOX_INTEGRALSOLVER_INC


#ifndef MAFOX_INTEGRALSOLVER_H
#define MAFOX_INTEGRALSOLVER_H


namespace mafox
{
    // >>> SKATCH: only for one variable <<<
    struct IntegralSolver
    {
        template 
        <
            typename XT,
            typename Function, 
            typename NodesCount = unsigned int, 
            typename Eps = double
            // TODO: typename LegendreRootsContainer = std::vector<typename metaxxa::Function<Function>::Result>
        >
        static auto gaussian
        (
            const Function &f,
            metaxxa::TypeOrRef<const XT> from,
            metaxxa::TypeOrRef<const XT> to,
            metaxxa::TypeOrRef<const NodesCount>, 
            metaxxa::TypeOrRef<const Eps> = MAFOX_DEFAULT_EPS
        );

        // TODO: with cache
    };

}

#endif // MAFOX_INTEGRALSOLVER_H


namespace mafox
{   
    template <typename XT, typename Function, typename NodesCount, typename Eps>
    auto IntegralSolver::gaussian
    (
        const Function &f, 
        metaxxa::TypeOrRef<const XT> from,
        metaxxa::TypeOrRef<const XT> to,
        metaxxa::TypeOrRef<const NodesCount> nodes, 
        metaxxa::TypeOrRef<const Eps> eps   
    )
    {
        
        using ResultT = double; // TODO Function::Result instead double

        LegendrePolynomial<XT> polynomial(nodes);

        // For variable replacing
        auto k      = (to - from) / 2;
        auto offset = (to + from) / 2;

        auto roots = polynomial.roots(eps);

        ResultT result = 0.0;
        ResultT weight = 0.0;
        ResultT derivative = 0.0;
        for(std::size_t i = 0; i < nodes; ++i)
        {
            derivative = polynomial.derivative(roots[i]);
            weight = 2.0/((1 - roots[i]*roots[i]) * derivative*derivative);

            result += weight * f(k*roots[i] + offset);
        }

        return k*result;
    }
}

#endif // MAFOX_INTEGRALSOLVER_INC

#endif // MAFOX_H


#endif // MAFOX_HPP