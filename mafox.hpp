#ifndef MAFOX_HPP
#define MAFOX_HPP

#include <type_traits>
#include <stdexcept>
#include <utility>
#include <functional>
#include <iostream>
#include <iomanip>
#include <memory>
#include <cstring>
#include <tuple>

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

        static constexpr Type value = LITERAL;
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
        using Tail = LiteralList<typename LiteralH::Type, LiteralTail::value...>;
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
        using IntegerSequence = std::integer_sequence<T, LITERALS...>;

        using HeadType = typename Head::Type;

        static constexpr HeadType head()
        {
            return Head::value;
        }
    };

    template <typename T>
    struct LiteralList<T> : public detail::LiteralListTag
    {
        using Head = LiteralNil;
        using Tail = LiteralNil;
        using IntegerSequence = std::integer_sequence<T>;

        using HeadType = LiteralNilT;

        static constexpr HeadType head()
        {
            return NIL;
        }
    };

    namespace detail
    {
        template 
        <
            template <typename T, T...> typename SequenceT,
            typename LiteralT, 
            LiteralT... LITERALS
        >
        constexpr auto literal_list_from_sequence(SequenceT<LiteralT, LITERALS...>)
            -> LiteralList<LiteralT, LITERALS...>;
    }

    template <typename Sequence>
    using LiteralListFromSequence = decltype(detail::literal_list_from_sequence(std::declval<Sequence>()));
}

namespace std
{
    template <typename T, T... LITERALS>
    class tuple_size<metaxxa::LiteralList<T, LITERALS...>>
        : public std::integral_constant<std::size_t, sizeof...(LITERALS)>
    {};

    template <typename T>
    class tuple_size<metaxxa::LiteralList<T>>
        : public std::integral_constant<std::size_t, 0>
    {};

    template <size_t INDEX, typename T, T... LITERALS>
	class tuple_element<INDEX, metaxxa::LiteralList<T, LITERALS...>>
        : public tuple_element<INDEX - 1, typename metaxxa::LiteralList<T, LITERALS...>::Tail>
	{};

    template <size_t INDEX>
	class tuple_element<INDEX, metaxxa::LiteralNil>
    {};

    template <>
	class tuple_element<0, metaxxa::LiteralNil>
    {};

    template <typename T, T... LITERALS>
	class tuple_element<0, metaxxa::LiteralList<T, LITERALS...>>
	{
	public:
		using type = typename metaxxa::LiteralList<T, LITERALS...>::Head;
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
        template <typename... StartTypes>
        struct Mover
        {
            template
            <
                template <typename...> typename DestTemplate,
                template <typename...> typename SrcTemplate,
                typename... Args
            >
            static constexpr auto move_parameters(SrcTemplate<Args...> &&) -> TypeSaver<DestTemplate<StartTypes..., Args...>>;
        };
    }

    template 
    <
        template <typename...> typename DestTemplate,
        typename SrcTemplate,
        typename... StartTypes
    >
    using MoveParameters = typename decltype
    (
        detail::Mover<StartTypes...>::template move_parameters<DestTemplate>(std::declval<SrcTemplate>())
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

        constexpr TypeTuple(const TypeTuple &) = default;

        constexpr TypeTuple(TypeTuple &&) = default;

        virtual ~TypeTuple() = default;

        TypeTuple &operator=(const TypeTuple &) = default;

        TypeTuple &operator=(TypeTuple &&) = default;

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


namespace metaxxa
{
    template <std::size_t INDEX>
    using SizeConstant = std::integral_constant<std::size_t, INDEX>;
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
        Functor<__VA_ARGS__>::value,                            \
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
        template <typename E>
        struct IsEqualTo
        {
            template <typename T>
            struct Type
            {
                static constexpr bool value = std::is_same_v<T, E>;
            };
        };

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
        typename T
    >
    using FindEqual = detail::Find<TupleT, detail::IsEqualTo<T>::template Type>;

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
                        Functor<std::tuple_element_t<INDICES, TupleT>>::value
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
                        Functor<std::tuple_element_t<INDICES, TupleT>, INDICES>::value
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
                        Functor<std::tuple_element_t<INDICES, TupleT>, INDICES, TupleT>::value
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

#ifndef METAXXA_ALGORITHM_EVERY_H
#define METAXXA_ALGORITHM_EVERY_H


namespace metaxxa
{
    namespace detail
    {
        template 
        <
            template <typename> typename Predicate,
            typename TupleT, 
            std::size_t... INDICES
        >
        constexpr bool every(std::index_sequence<INDICES...>)
        {
            return (true && ... && Predicate<std::tuple_element_t<INDICES, TupleT>>::value);
        }
    }

    template
    <
        template <typename> typename Predicate,
        typename TupleT
    >
    struct Every 
        : public std::bool_constant
        <
            detail::every<Predicate, TupleT>(std::make_index_sequence<std::tuple_size_v<TupleT>>())
        >
    {};

    template
    <
        template <typename> typename Predicate,
        typename TupleT
    >
    constexpr bool every = Every<Predicate, TupleT>::value;
}

#endif // METAXXA_ALGORITHM_EVERY_H

#ifndef METAXXA_CONTAINS_H
#define METAXXA_CONTAINS_H



namespace metaxxa
{
    template
    <
        typename TupleT,
        typename... Types
    >
    constexpr bool contains_all();

    namespace detail
    {
        template
        <
            typename TupleT,
            typename Type,
            std::size_t... INDICES
        >
        constexpr bool contains(std::index_sequence<INDICES...>)
        {
            return (false || ... || std::is_same_v<std::tuple_element_t<INDICES, TupleT>, Type>);
        }

        template
        <
            typename TupleT,
            typename RequestedTuple,
            std::size_t... INDICES
        >
        constexpr bool contains_packed(std::index_sequence<INDICES...>)
        {
            return contains_all<TupleT, std::tuple_element_t<INDICES, RequestedTuple>...>();
        }
    }

    template
    <
        typename TupleT,
        typename Type
    >
    constexpr bool contains()
    {
        using Tuple = MoveParameters<TypeTuple, TupleT>;        

        return detail::contains<Tuple, Type>(std::make_index_sequence<std::tuple_size_v<Tuple>>());
    }

    template
    <
        typename TupleT,
        typename... Types
    >
    constexpr bool contains_all()
    {
        using Tuple = MoveParameters<TypeTuple, TupleT>;

        return (true && ... && contains<Tuple, Types>());
    }

    template
    <
        typename TupleT,
        typename RequestedTuple
    >
    constexpr bool contains_packed()
    {
        using Tuple = MoveParameters<TypeTuple, TupleT>;
        using Requested = MoveParameters<TypeTuple, RequestedTuple>;

        return detail::contains_packed<Tuple, Requested>(std::make_index_sequence<std::tuple_size_v<Requested>>());
    }

    template <typename TupleT, typename T>
    using Contains = typename If<contains<TupleT, T>()>
        ::template Then<std::true_type>
        ::template Else<std::false_type>
        ::Type;

    template <typename TupleT, typename... Ts>
    using ContainsAll = typename If<contains_all<TupleT, Ts...>()>
        ::template Then<std::true_type>
        ::template Else<std::false_type>
        ::Type;

    template <typename TupleT, typename RequestedTupleT>
    using ContainsPacked = typename If<contains_packed<TupleT, RequestedTupleT>()>
        ::template Then<std::true_type>
        ::template Else<std::false_type>
        ::Type;
}

#endif // METAXXA_CONTAINS_H

#ifndef METAXXA_UNIQUE_H
#define METAXXA_UNIQUE_H


namespace metaxxa
{
    namespace detail
    {
        template 
        <
            template <typename...> typename ResultTemplate, 
            typename ResultTuple, 
            typename T, 
            typename... Rest
        >
        class Unique
        {
            static constexpr bool CONTAINS = metaxxa::contains<ResultTuple, T>();

            using NextUnique = typename If<CONTAINS>
                ::template Then
                <
                    Unique<ResultTemplate, ResultTuple, Rest...>
                >
                ::template Else
                <
                    Unique
                    <
                        ResultTemplate, 
                        typename ResultTuple::template Concat<TypeTuple<T>>, 
                        Rest...
                    >
                >
                ::Type;
        public:
            using Type = typename NextUnique::Type;

            using CheckType = typename If<CONTAINS>
                ::template Then<std::false_type>
                ::template Else<typename NextUnique::CheckType>
                ::Type;
        };

        template 
        <
            template <typename...> typename ResultTemplate,
            typename ResultTuple, 
            typename T
        >
        class Unique<ResultTemplate, ResultTuple, T>
        {
            static constexpr bool CONTAINS = metaxxa::contains<ResultTuple, T>();

            using FinalTuple = typename If<CONTAINS>
                ::template Then<ResultTuple>
                ::template Else<typename ResultTuple::template Concat<TypeTuple<T>>>
                ::Type;

        public:
            using Type = MoveParameters<ResultTemplate, FinalTuple>;

            using CheckType = typename If<CONTAINS>
                ::template Then<std::false_type>
                ::template Else<std::true_type>
                ::Type;
        };

        template 
        <
            template <typename...> typename ResultTemplate,
            typename... Types
        >
        struct AvoidEmpty
        {
            using Type = typename Unique<ResultTemplate, TypeTuple<>, Types...>::Type;

            using CheckType = typename Unique<ResultTemplate, TypeTuple<>, Types...>::CheckType;
        };

        template <template <typename...> typename ResultTemplate>
        struct AvoidEmpty<ResultTemplate>
        {
            using Type = ResultTemplate<>;

            using CheckType = std::true_type;
        };

        template <template <typename...> typename ResultTemplate>
        struct UniqueGate
        {
            template <typename... Types>
            struct Type
            {
                using Result = typename AvoidEmpty<ResultTemplate, Types...>::Type;
            };

            template <typename... Types>
            using CheckType = typename AvoidEmpty<ResultTemplate, Types...>::CheckType;
        };
    }

    template
    <
        template <typename...> typename ResultTemplate,
        typename TupleT
    >
    using Unique = typename MoveParameters
    <
        detail::UniqueGate<ResultTemplate>::template Type,
        TupleT
    >::Result;

    template <typename TupleT>
    using IsUnique = MoveParameters
    <
        detail::UniqueGate<TypeTuple>::template CheckType,
        TupleT
    >;

    template <typename TupleT>
    constexpr bool is_unique()
    {
        return IsUnique<TupleT>::value;
    }
}

#endif // METAXXA_UNIQUE_H

#ifndef METAXXA_ALGORITHM_INVOKEFUNCTIONS_INC
#define METAXXA_ALGORITHM_INVOKEFUNCTIONS_INC


#ifndef METAXXA_ALGORITHM_INVOKEFUNCTIONS_H
#define METAXXA_ALGORITHM_INVOKEFUNCTIONS_H



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
            static constexpr bool value = T::value;
        };

        template <bool RESULT, typename CaseType>
        struct CasePair
        {
            static constexpr bool value = RESULT;

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

#ifndef METAXXA_ISINSTANTIATIONOF_H
#define METAXXA_ISINSTANTIATIONOF_H


namespace metaxxa
{
    namespace detail
    {
        template 
        <
            template <typename...> typename TemplateType, 
            template <typename...> typename SrcType, 
            typename... Types
        >
        constexpr auto is_instatiation_of(SrcType<Types...> &&)
            -> std::bool_constant<std::is_same_v<TemplateType<Types...>, SrcType<Types...>>>;

        template 
        <
            template <typename...> typename TemplateType,
            typename SrcType,
            typename... Types
        >
        constexpr auto is_instatiation_of(SrcType &&)
            -> std::false_type;
    }

    template <typename T, template <typename...> typename TemplateType>
    constexpr bool is_instatiation_of()
    {
        return decltype(detail::is_instatiation_of<TemplateType>(std::declval<T>()))::value;
    }
}

#endif // METAXXA_ISINSTANTIATIONOF_H

#ifndef METAXXA_ISARRAYOF_H
#define METAXXA_ISARRAYOF_H


namespace metaxxa
{
    template <typename T, typename Of>
    constexpr bool is_array_of()
    {
        using TNCV = std::remove_cv_t<std::remove_reference_t<T>>;
        using OfNCV = std::remove_cv_t<std::remove_reference_t<Of>>;

        return std::is_array_v<TNCV> 
        && std::is_same_v
        <
            TNCV, 
            OfNCV[std::extent_v<TNCV>]
        >;
    }    
}

#endif // METAXXA_ISARRAYOF_H


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

#define MAFOX_DEFAULT_EPS 0.001

namespace mafox
{
    class FatalError : public std::runtime_error
    {
    public:
        using std::runtime_error::runtime_error;
    };
}

#define MAFOX_FATAL(message) do { assert(false && message); throw mafox::FatalError(message); } while(0)

#endif // MAFOX_DETAIL_DEF_H

#ifndef MAFOX_UTIL_INC
#define MAFOX_UTIL_INC


#ifndef MAFOX_UTIL_H
#define MAFOX_UTIL_H


namespace mafox
{
    template <typename T>
    inline const static T ZERO(0);

    mafox_inline void zero_array(void *s, size_t n);
}

#endif // MAFOX_UTIL_H

namespace mafox
{
    mafox_inline void zero_array(void *s, size_t n)
    {
        volatile char *p = static_cast<volatile char *>(s);
        while (n--) *p++ = 0;
    }
}

#endif // MAFOX_UTIL_INC

#ifndef MAFOX_AMATRIX_INC
#define MAFOX_AMATRIX_INC


#ifndef MAFOX_AMATRIX_H
#define MAFOX_AMATRIX_H



#ifndef MAFOX_IMATRIX_H
#define MAFOX_IMATRIX_H


namespace mafox
{
    template <typename T>
    class IMatrix
    {
    public:
        using difference_type     = std::ptrdiff_t;
        using value_type          = std::remove_cv_t<T>;
        using pointer             = T *;
        using const_pointer       = const T *;
        using reference           = T &;
        using const_reference     = const T &;

        IMatrix() = default;

        IMatrix(const IMatrix &) = default;

        IMatrix(IMatrix &&) = default;

        virtual ~IMatrix() = default;

        IMatrix &operator=(const IMatrix &) = delete;

        IMatrix &operator=(IMatrix &&) = delete;

        virtual std::size_t rows() const = 0;

        virtual std::size_t cols() const = 0;

        virtual reference element(std::size_t i, std::size_t j) = 0;

        virtual const_reference element(std::size_t i, std::size_t j) const = 0;

        virtual void set_element(std::size_t i, std::size_t j, const_reference) = 0;

        virtual void transpose() = 0;

        virtual void transpose_rsd() = 0;

        virtual std::shared_ptr<IMatrix> share_interface() = 0;

        virtual std::shared_ptr<const IMatrix> share_interface() const = 0;
    };
}

#endif // MAFOX_IMATRIX_H

#ifndef MAFOX_SIZE_H
#define MAFOX_SIZE_H


namespace mafox
{
    template <typename T>
    struct Size2D
    {
        T width;
        T height;
    };

    template <typename T>
    struct Size3D
    {
        T width;
        T height;
        T length;
    };
}

#endif // MAFOX_SIZE_H

#define USING_MAFOX_MATRIX_TYPES(Matrix)                                         \
    using data_t              = typename AMatrix<Matrix>::data_t;                \
    using shared_data_t       = typename AMatrix<Matrix>::shared_data_t;         \
    using const_shared_data_t = typename AMatrix<Matrix>::const_shared_data_t;   \
    using difference_type_t   = typename AMatrix<Matrix>::difference_type;       \
    using value_type          = typename AMatrix<Matrix>::value_type;            \
    using pointer             = typename AMatrix<Matrix>::pointer;               \
    using const_pointer       = typename AMatrix<Matrix>::const_pointer;         \
    using reference           = typename AMatrix<Matrix>::reference;             \
    using const_reference     = typename AMatrix<Matrix>::const_reference;       \
    using Size                = typename AMatrix<Matrix>::Size

#define MAFOX_DEFAULT_TRAITS(value_t, user_data_t)                 \
        using data_t              = user_data_t;                   \
        using shared_data_t       = std::shared_ptr<data_t>;       \
        using const_shared_data_t = std::shared_ptr<const data_t>; \
        using difference_type     = std::ptrdiff_t;                \
        using value_type          = std::remove_cv_t<value_t>;     \
        using pointer             = value_t *;                     \
        using const_pointer       = const value_t *;               \
        using reference           = value_t &;                     \
        using const_reference     = const value_t &

#define MAFOX_INHERIT_TRAITS(base_t)                                      \
        using data_t              = typename base_t::data_t;              \
        using shared_data_t       = typename base_t::shared_data_t;       \
        using const_shared_data_t = typename base_t::const_shared_data_t; \
        using difference_type     = typename base_t::difference_type;     \
        using value_type          = typename base_t::value_type;          \
        using pointer             = typename base_t::pointer;             \
        using const_pointer       = typename base_t::const_pointer;       \
        using reference           = typename base_t::reference;           \
        using const_reference     = typename base_t::const_reference

namespace mafox
{
    template <typename Matrix>
    struct MatrixTraits;

    template <typename Matrix>
    class AMatrix : public IMatrix<typename MatrixTraits<Matrix>::value_type>
    {
    public:
        using Traits              = MatrixTraits<Matrix>;
        using data_t              = typename Traits::data_t;
        using shared_data_t       = typename Traits::shared_data_t;
        using const_shared_data_t = typename Traits::const_shared_data_t;

        using value_type          = typename Traits::value_type;
        using Interface           = IMatrix<value_type>;
        using difference_type     = typename Interface::difference_type;
        using pointer             = typename Interface::pointer;
        using const_pointer       = typename Interface::const_pointer;
        using reference           = typename Interface::reference;
        using const_reference     = typename Interface::const_reference;
        
        using Size                = Size2D<std::size_t>;

        AMatrix() = default;

        AMatrix(const AMatrix &) = default;

        AMatrix(AMatrix &&) = default;

        virtual ~AMatrix() = default;

        AMatrix &operator=(const AMatrix &) = delete;

        AMatrix &operator=(AMatrix &&) = delete;

        Size size() const;

        bool is_square() const;

        mafox_inline const_reference operator()(std::size_t i, std::size_t j) const;

        virtual Matrix transposed() = 0;

        virtual Matrix transposed_rsd() = 0;

        virtual bool try_set_element(std::size_t i, std::size_t j, const_reference);

        virtual shared_data_t shared_data() = 0;

        virtual const_shared_data_t shared_cdata() const = 0;

        virtual Matrix share() = 0;
    };
}

template <typename T>
std::ostream &operator<<(std::ostream &, const mafox::AMatrix<T> &);

#endif // MAFOX_AMATRIX_H

namespace mafox
{
    template <typename Matrix>
    typename AMatrix<Matrix>::Size AMatrix<Matrix>::size() const
    {
        return Size { this->rows(), this->cols() };
    }

    template <typename Matrix>
    bool AMatrix<Matrix>::is_square() const
    {
        return this->rows() == this->cols();
    }

    template <typename Matrix>
    mafox_inline typename AMatrix<Matrix>::const_reference 
    AMatrix<Matrix>::operator()(std::size_t i, std::size_t j) const
    {
        return this->element(i, j);
    }

    template <typename Matrix>
    bool AMatrix<Matrix>::try_set_element(std::size_t i, std::size_t j, const_reference value)
    {
        this->set_element(i, j, value);
        return true;
    }
}

template <typename T>
std::ostream &operator<<(std::ostream &os, const mafox::AMatrix<T> &matrix)
{
    std::streamsize width = os.width();
    std::streamsize precision = os.precision();

    for(std::size_t i = 0, j = 0; i < matrix.rows(); ++i)
    {
        for(j = 0; j < matrix.cols(); ++j)
            os << std::setw(width) << std::setprecision(precision) << matrix.element(i, j) << ' ';
        os << '\n';
    }

    return os << std::flush;
}

#endif // MAFOX_AMATRIX_INC

#ifndef MAFOX_MATRIX_INC
#define MAFOX_MATRIX_INC



#ifndef MAFOX_MATRIX_H
#define MAFOX_MATRIX_H



#ifndef MAFOX_MATRIXORDER_H
#define MAFOX_MATRIXORDER_H

namespace mafox
{
    enum MatrixOrder
    {
        ROW_MAJOR,
        COL_MAJOR
    };
}

#endif // MAFOX_MATRIXORDER_H

namespace mafox
{
    template <typename T>
    class matrix_data_t;

    template <typename T>
    class Matrix;

    template <typename T>
    struct MatrixTraits<Matrix<T>>
    {
        MAFOX_DEFAULT_TRAITS(T, matrix_data_t<T>);
    };

    template <typename T>
    class Matrix : public AMatrix<Matrix<T>>
    {
    public:
        USING_MAFOX_MATRIX_TYPES(Matrix);

        Matrix(std::size_t rows, std::size_t cols, MatrixOrder = MatrixOrder::ROW_MAJOR);

        // template <typename Iterator>
        // Matrix(Iterator begin, Iterator end);

        // Matrix(std::initializer_list<std::initializer_list<T>>);

        Matrix(const Matrix &);

        Matrix(Matrix &&);

        virtual ~Matrix();

        Matrix &operator=(const Matrix &);

        Matrix &operator=(Matrix &&);

        mafox_inline virtual std::size_t rows() const override;

        mafox_inline virtual std::size_t cols() const override;

        virtual reference element(std::size_t i, std::size_t j) override;

        virtual const_reference element(std::size_t i, std::size_t j) const override;

        virtual void set_element(std::size_t i, std::size_t j, const_reference) override;

        virtual void transpose() override;

        virtual Matrix<T> transposed() override;

        virtual void transpose_rsd() override;

        virtual Matrix<T> transposed_rsd() override;

        virtual shared_data_t shared_data() override;

        virtual const_shared_data_t shared_cdata() const override;

        virtual Matrix<T> share() override;

        virtual std::shared_ptr<IMatrix<T>> share_interface() override;

        virtual std::shared_ptr<const IMatrix<T>> share_interface() const override;

        pointer data();

        const_pointer cdata() const;

        MatrixOrder order() const;

        void set_order(MatrixOrder);

    private:
        Matrix(shared_data_t);

        shared_data_t m_data;
    };
}

#endif // MAFOX_MATRIX_H

namespace mafox
{
    template <typename T>
    class matrix_data_t
    {
        matrix_data_t(std::size_t rows, std::size_t cols, MatrixOrder order, std::unique_ptr<T[]> &&array)
        : rows(rows), cols(cols), order(order), array(std::forward<std::unique_ptr<T[]>>(array))
        {}

        matrix_data_t(std::size_t rows, std::size_t cols, MatrixOrder order)
        : rows(rows), cols(cols), order(order), array(new T[rows*cols])
        {}
        
    public:
        static auto make(std::size_t rows, std::size_t cols, MatrixOrder order, std::unique_ptr<T[]> &&array)
        {
            return std::shared_ptr<matrix_data_t<T>>
            (
                new matrix_data_t<T>
                (
                    rows, 
                    cols, 
                    order, 
                    std::forward<std::unique_ptr<T[]>>(array)
                )
            );
        }

        static auto make(std::size_t rows, std::size_t cols, MatrixOrder order)
        {
            return std::shared_ptr<matrix_data_t<T>>
            (
                new matrix_data_t<T>
                (
                    rows, 
                    cols, 
                    order
                )
            );
        }

        // TODO: для resize и оператора присваивания обеспечить безопасность в многопоточной среде
        // Важно, чтобы во время, например, транспонирования, умножения и проч. не происходило изменения размера матрицы.
        // См. читатель-писатель 

        std::size_t rows;
        std::size_t cols;
        MatrixOrder order;

        std::unique_ptr<T[]> array; 
    };

    template <typename T>
    Matrix<T>::Matrix(std::size_t rows, std::size_t cols, MatrixOrder order)
    : m_data(matrix_data_t<T>::make(rows, cols, order))
    {
        std::size_t sz = rows * cols * sizeof(T);

        zero_array(data(), sz);
    }

    // template <typename T>
    // template <typename Iterator>
    // Matrix<T>::Matrix(Iterator begin, Iterator end)
    // {}

    // template <typename T>
    // Matrix<T>::Matrix(std::initializer_list<std::initializer_list<T>> list)
    // {}

    template <typename T>
    Matrix<T>::Matrix(const Matrix &other)
    : m_data(matrix_data_t<T>::make(other.rows(), other.cols(), other.order()))
    {
        memcpy(m_data->array.get(), other.m_data->array.get(), m_data->rows * m_data->cols * sizeof(T));
    }

    template <typename T>
    Matrix<T>::Matrix(Matrix &&other)
    : m_data(matrix_data_t<T>::make(other.rows(), other.cols(), other.order(), std::move(other.m_data->array)))
    {}

    template <typename T>
    Matrix<T>::Matrix(shared_data_t m_data)
    : m_data(m_data)
    {}

    template <typename T>
    Matrix<T>::~Matrix()
    {}

    template <typename T>
    Matrix<T> &Matrix<T>::operator=(const Matrix &rhs)
    {
        if(this != &rhs)
            *this = std::move(Matrix<T>(rhs));
        
        return *this;
    }

    template <typename T>
    Matrix<T> &Matrix<T>::operator=(Matrix &&rhs)
    {
        if(this != &rhs)
        {
            m_data->rows  = rhs.m_data->rows;
            m_data->cols  = rhs.m_data->cols;
            m_data->order = rhs.m_data->order;
            m_data->array = std::move(rhs.m_data->array);
        }
        
        return *this;
    }

    template <typename T>
    mafox_inline std::size_t Matrix<T>::rows() const
    {
        return m_data->rows;
    }

    template <typename T>
    mafox_inline std::size_t Matrix<T>::cols() const
    {
        return m_data->cols;
    }

    template <typename T>
    typename Matrix<T>::reference Matrix<T>::element(std::size_t i, std::size_t j)
    {
        assert(i < rows() && j < cols());

        if(order() == ROW_MAJOR)
            return data()[i*cols() + j];
        else if(order() == COL_MAJOR)
            return data()[j*rows() + i];
        else
            MAFOX_FATAL("Unknown matrix order");
    }

    template <typename T>
    typename Matrix<T>::const_reference Matrix<T>::element(std::size_t i, std::size_t j) const
    {
        return const_cast<Matrix<T>*>(this)->element(i, j);
    }

    template <typename T>
    void Matrix<T>::set_element(std::size_t i, std::size_t j, const_reference value)
    {
        element(i, j) = value;
    }

    template <typename T>
    void Matrix<T>::transpose()
    {
        if(this->is_square())
        {
            std::size_t sz = rows();
            for(std::size_t i = 1, j = 0, diff = 0; i < sz; ++i)
                for(j = i; j > 0; --j)
                {
                    diff = i-j;
                    std::swap(element(i, diff), element(diff, i));
                }

            std::swap(m_data->rows, m_data->cols);        
        }
        else
            *this = std::move(transposed());
    }

    template <typename T>
    Matrix<T> Matrix<T>::transposed()
    {
        std::size_t r = rows(), c = cols();
        Matrix<T> result(c, r);

        for(std::size_t i = 0, j = 0; i < r; ++i)
            for(j = 0; j < c; ++j)
                result.set_element(j, i, element(i, j));

        return result;
    }

    template <typename T>
    void Matrix<T>::transpose_rsd()
    {
        if(this->is_square())
        {
            std::size_t sz = rows();
            for(std::size_t i = 0, j = 0, diff = 0; i < sz; ++i)
                for(diff = sz-i-1, j = diff; j > 0; --j)
                {
                    std::swap(element(diff-j, i), element(diff, i+j));
                }

            std::swap(m_data->rows, m_data->cols);        
        }
        else
            *this = std::move(transposed_rsd());
    }

    template <typename T>
    Matrix<T> Matrix<T>::transposed_rsd()
    {
        std::size_t r = rows(), c = cols();
        Matrix<T> result(c, r);

        for(std::size_t i = 0, j = 0; i < r; ++i)
            for(j = 0; j < c; ++j)
                result.set_element(c - j - 1, r - i - 1, element(i, j));

        return result;
    }

    template <typename T>
    typename Matrix<T>::shared_data_t Matrix<T>::shared_data()
    {
        return m_data;
    }

    template <typename T>
    typename Matrix<T>::const_shared_data_t Matrix<T>::shared_cdata() const
    {
        return m_data;
    }

    template <typename T>
    Matrix<T> Matrix<T>::share()
    {
        return Matrix<T>(m_data);
    }

    template <typename T>
    std::shared_ptr<IMatrix<T>> Matrix<T>::share_interface()
    {
        return std::shared_ptr<Matrix<T>>(new Matrix<T>(m_data));
    }

    template <typename T>
    std::shared_ptr<const IMatrix<T>> Matrix<T>::share_interface() const
    {
        return std::shared_ptr<Matrix<T>>(new Matrix<T>(m_data));
    }

    template <typename T>
    typename Matrix<T>::pointer Matrix<T>::data()
    {
        return m_data->array.get();
    }

    template <typename T>
    typename Matrix<T>::const_pointer Matrix<T>::cdata() const
    {
        return m_data->array.get();
    }

    template <typename T>
    MatrixOrder Matrix<T>::order() const
    {
        return m_data->order;
    }

    template <typename T>
    void Matrix<T>::set_order(MatrixOrder order)
    {
        if(m_data->order != order)
        {
            std::size_t r = rows(), c = cols();
            Matrix<T> reordered(r, c, order);
            for(std::size_t i = 0, j = 0; i < r; ++i)
                for(j = 0; j < c; ++j)
                    reordered.set_element(i, j, element(i, j));

            *this = std::move(reordered);
        }
    }
}

#endif // MAFOX_MATRIX_INC

#ifndef MAFOX_BANDMATRIX_INC
#define MAFOX_BANDMATRIX_INC


#ifndef MAFOX_BANDMATRIX_H
#define MAFOX_BANDMATRIX_H



namespace mafox
{
    template <typename T>
    class band_matrix_data_t;

    template <typename T>
    class BandMatrix;

    template <typename T>
    struct MatrixTraits<BandMatrix<T>>
    {
        MAFOX_DEFAULT_TRAITS(T, band_matrix_data_t<T>);
    };

    template <typename T>
    class BandMatrix : public AMatrix<BandMatrix<T>>
    {
    public:
        USING_MAFOX_MATRIX_TYPES(BandMatrix);

        BandMatrix(std::size_t size, std::size_t lower_bandwidth, std::size_t upper_bandwidth);

        BandMatrix(const BandMatrix &);

        BandMatrix(BandMatrix &&);

        virtual ~BandMatrix();

        BandMatrix &operator=(const BandMatrix &);

        BandMatrix &operator=(BandMatrix &&);

        // template <typename Iterator>
        // BandMatrix(Iterator begin, Iterator end);

        // BandMatrix(std::initializer_list<std::initializer_list<T>>);

        virtual std::size_t rows() const override;

        virtual std::size_t cols() const override;

        virtual const_reference element(std::size_t i, std::size_t j) const override;

        virtual void set_element(std::size_t i, std::size_t j, const_reference) override;
        
        virtual bool try_set_element(std::size_t i, std::size_t j, const_reference) override;

        virtual void transpose() override;

        virtual BandMatrix<T> transposed() override;

        virtual void transpose_rsd() override;

        virtual BandMatrix<T> transposed_rsd() override;

        virtual shared_data_t shared_data() override;

        virtual const_shared_data_t shared_cdata() const override;

        virtual BandMatrix<T> share() override;

        virtual std::shared_ptr<IMatrix<T>> share_interface() override;

        virtual std::shared_ptr<const IMatrix<T>> share_interface() const override;

        std::size_t lower_bandwidth() const;

        std::size_t upper_bandwidth() const;

        pointer diagonal_data();

        const_pointer diagonal_cdata() const;

        pointer lower_diagonal_data(std::size_t level);

        const_pointer lower_diagonal_cdata(std::size_t level) const;

        pointer upper_diagonal_data(std::size_t level);

        const_pointer upper_diagonal_cdata(std::size_t level) const;

    private:
        virtual reference element(std::size_t i, std::size_t j) override;

        BandMatrix(shared_data_t);

        shared_data_t m_data;
    };

    template <typename Iterator>
    BandMatrix(Iterator begin, Iterator end) -> BandMatrix<typename Iterator::value_type>;
}

#endif // MAFOX_BANDMATRIX_H

namespace mafox
{
    template <typename T>
    class band_matrix_data_t
    {
    public:
        band_matrix_data_t(std::size_t size, std::size_t l, std::size_t u)
        : size(size), l(l), u(u), arrays(new std::unique_ptr<T[]>[1+l+u])
        {
            long long low = static_cast<long long>(l);
            long long up = static_cast<long long>(u);

            long long col = 0;
            long long i = -low;
            for(std::size_t diag_sz = size+i; i <= up; ++i, ++col, diag_sz = size-std::abs(i))
            {
                arrays[col].reset(new T[diag_sz]);
                zero_array(arrays[col].get(), diag_sz * sizeof(T));
            }
        }

        band_matrix_data_t(band_matrix_data_t &other)
        : size(other.size), l(other.l), u(other.u), arrays(new std::unique_ptr<T[]>[1+l+u])
        {
            long long low = static_cast<long long>(l);
            long long up = static_cast<long long>(u);

            long long col = 0;
            long long i = -low;
            for(std::size_t diag_sz = size+i; i <= up; ++i, ++col, diag_sz = size-std::abs(i))
            {
                arrays[col].reset(new T[diag_sz]);
                memcpy(arrays[col].get(), other.arrays[col].get(), diag_sz * sizeof(T));
            }
        }

        band_matrix_data_t(band_matrix_data_t &&other)
        : size(other.size), l(other.l), u(other.u), arrays(std::move(other.arrays)) 
        {}

        static auto make(std::size_t size, std::size_t l, std::size_t u)
        {
            return std::make_shared<band_matrix_data_t<T>>(size, l, u);
        }

        static auto make(band_matrix_data_t &other)
        {
            return std::make_shared<band_matrix_data_t<T>>(other);
        }

        static auto make(band_matrix_data_t &&other)
        {
            return std::make_shared<band_matrix_data_t<T>>(std::forward<band_matrix_data_t>(other));
        }

        bool is_zero(std::size_t i, std::size_t j) const
        {
            return static_cast<long long>(j) < static_cast<long long>(i) - static_cast<long long>(l) 
                || j > i + u;
        }

        const T &c_at(std::size_t i, std::size_t j) const
        {
            if(is_zero(i, j))
                return ZERO<T>;
            else
            {
                std::size_t col = j - (static_cast<long long>(i) - static_cast<long long>(l));
                std::size_t row = (j < i? i - (i-j) : i);

                return arrays[col][row];
            }
        }

        T &at(std::size_t i, std::size_t j)
        {
            assert(!is_zero(i, j));

            std::size_t col = j - (static_cast<long long>(i) - static_cast<long long>(l));
            std::size_t row = (j < i? i - (i-j) : i);

            return arrays[col][row];
        }

        std::size_t size;
        std::size_t l, u;
        std::unique_ptr<std::unique_ptr<T[]>[]> arrays;
    };

    template <typename T>
    BandMatrix<T>::BandMatrix(std::size_t size, std::size_t lower_bandwidth, std::size_t upper_bandwidth)
    : m_data(band_matrix_data_t<T>::make(size, lower_bandwidth, upper_bandwidth))
    {
        assert(lower_bandwidth < size && upper_bandwidth < size);
    }

    template <typename T>
    BandMatrix<T>::BandMatrix(const BandMatrix &other)
    : m_data(band_matrix_data_t<T>::make(*other.m_data))
    {}

    template <typename T>
    BandMatrix<T>::BandMatrix(BandMatrix &&other)
    : m_data(std::move(other.m_data))
    {}

    template <typename T>
    BandMatrix<T>::BandMatrix(shared_data_t m_data)
    : m_data(m_data)
    {}

    template <typename T>
    BandMatrix<T>::~BandMatrix()
    {}

    template <typename T>
    BandMatrix<T> &BandMatrix<T>::operator=(const BandMatrix &rhs)
    {
        if(this != &rhs)
            *this = std::move(BandMatrix<T>(rhs));

        return *this;
    }

    template <typename T>
    BandMatrix<T> &BandMatrix<T>::operator=(BandMatrix &&rhs)
    {
        if(this != &rhs)
        {
            m_data->size   = rhs.m_data->size;
            m_data->l      = rhs.m_data->l;
            m_data->u      = rhs.m_data->u;
            m_data->arrays = std::move(rhs.m_data->arrays);
        }

        return *this;
    }

    template <typename T>
    std::size_t BandMatrix<T>::rows() const
    {
        return m_data->size;
    }

    template <typename T>
    std::size_t BandMatrix<T>::cols() const
    {
        return m_data->size;
    }

    template <typename T>
    typename BandMatrix<T>::reference BandMatrix<T>::element(std::size_t i, std::size_t j)
    {
        assert(i < rows() && j < cols());

        if(m_data->is_zero(i, j))
            MAFOX_FATAL("Accessing non-const zero element of banded matrix");
        else
            return m_data->at(i, j);
    }

    template <typename T>
    typename BandMatrix<T>::const_reference BandMatrix<T>::element(std::size_t i, std::size_t j) const
    {
        assert(i < rows() && j < cols());

        return m_data->c_at(i, j);
    }

    template <typename T>
    void BandMatrix<T>::set_element(std::size_t i, std::size_t j, const_reference value)
    {
        assert(try_set_element(i, j, value));
    }
    
    template <typename T>
    bool BandMatrix<T>::try_set_element(std::size_t i, std::size_t j, const_reference value)
    {
        assert(i < rows() && j < cols());

        if(m_data->is_zero(i, j))
            return false;
        else
        {
            m_data->at(i, j) = value;
            return true;
        }
    }

    template <typename T>
    void BandMatrix<T>::transpose()
    {
        if(lower_bandwidth() == upper_bandwidth())
        {
            for(std::size_t i = 1, diag_index = lower_bandwidth(); i <= diag_index; ++i)
                m_data->arrays[diag_index - i].swap(m_data->arrays[diag_index + i]);
            std::swap(m_data->l, m_data->u);
        }
        else
        {
            BandMatrix<T> result(m_data->size, m_data->u, m_data->l);
            
            for(std::size_t i = 0, sz = m_data->u+m_data->l; i <= sz; ++i)
                result.m_data->arrays[i] = std::move(m_data->arrays[sz - i]);

            *this = std::move(result);
        }
    }

    template <typename T>
    BandMatrix<T> BandMatrix<T>::transposed()
    {
        BandMatrix<T> result(*this);
        result.transpose();

        return result;
    }

    template <typename T>
    void BandMatrix<T>::transpose_rsd()
    {
        long long i = -static_cast<long long>(m_data->l);
        long long u = static_cast<long long>(m_data->u);
        std::size_t col = 0;

        for(; i <= u; ++i, ++col)
            std::reverse(m_data->arrays[col].get(), m_data->arrays[col].get() + m_data->size-std::abs(i));
    }

    template <typename T>
    BandMatrix<T> BandMatrix<T>::transposed_rsd()
    {
        BandMatrix<T> result(*this);
        result.transpose_rsd();

        return result;
    }

    template <typename T>
    typename BandMatrix<T>::shared_data_t BandMatrix<T>::shared_data()
    {
        return m_data;
    }

    template <typename T>
    typename BandMatrix<T>::const_shared_data_t BandMatrix<T>::shared_cdata() const
    {
        return m_data;
    }

    template <typename T>
    BandMatrix<T> BandMatrix<T>::share()
    {
        return BandMatrix<T>(m_data);
    }

    template <typename T>
    std::shared_ptr<IMatrix<T>> BandMatrix<T>::share_interface()
    {
        return std::shared_ptr<IMatrix<T>>(new BandMatrix<T>(m_data));
    }

    template <typename T>
    std::shared_ptr<const IMatrix<T>> BandMatrix<T>::share_interface() const
    {
        return std::shared_ptr<const IMatrix<T>>(new BandMatrix<T>(m_data));
    }

    template <typename T>
    std::size_t BandMatrix<T>::lower_bandwidth() const
    {
        return m_data->l;
    }

    template <typename T>
    std::size_t BandMatrix<T>::upper_bandwidth() const
    {
        return m_data->u;
    }

    template <typename T>
    typename BandMatrix<T>::pointer BandMatrix<T>::diagonal_data()
    {
        return m_data->arrays[m_data->l].get();
    }

    template <typename T>
    typename BandMatrix<T>::const_pointer BandMatrix<T>::diagonal_cdata() const
    {
        return const_cast<BandMatrix<T>*>(this)->diagonal_data();
    }

    template <typename T>
    typename BandMatrix<T>::pointer BandMatrix<T>::lower_diagonal_data(std::size_t level)
    {
        assert(level < lower_bandwidth());

        return m_data->arrays[m_data->l - (level + 1)].get();
    }

    template <typename T>
    typename BandMatrix<T>::const_pointer BandMatrix<T>::lower_diagonal_cdata(std::size_t level) const
    {
        return const_cast<BandMatrix<T>*>(this)->lower_diagonal_data(level);
    }

    template <typename T>
    typename BandMatrix<T>::pointer BandMatrix<T>::upper_diagonal_data(std::size_t level)
    {
        assert(level < upper_bandwidth());

        return m_data->arrays[m_data->l + level + 1].get();
    }

    template <typename T>
    typename BandMatrix<T>::const_pointer BandMatrix<T>::upper_diagonal_cdata(std::size_t level) const
    {
        return const_cast<BandMatrix<T>*>(this)->upper_diagonal_data(level);
    }
}

#endif // MAFOX_BANDMATRIX_INC

#ifndef MAFOX_TRIDIAGONALMATRIX_INC
#define MAFOX_TRIDIAGONALMATRIX_INC


#ifndef MAFOX_TRIDIAGONALMATRIX_H
#define MAFOX_TRIDIAGONALMATRIX_H


namespace mafox
{
    template <typename T>
    class TridiagonalMatrix : public BandMatrix<T>
    {
    public:
        USING_MAFOX_MATRIX_TYPES(BandMatrix<T>);

        TridiagonalMatrix(std::size_t size);

        TridiagonalMatrix(const TridiagonalMatrix &) = default;

        TridiagonalMatrix(TridiagonalMatrix &&) = default;

        virtual ~TridiagonalMatrix() = default;

        TridiagonalMatrix &operator=(const TridiagonalMatrix &) = default;

        TridiagonalMatrix &operator=(TridiagonalMatrix &&) = default;

        pointer lower_diagonal_data();

        const_pointer lower_diagonal_cdata() const;

        pointer upper_diagonal_data();

        const_pointer upper_diagonal_cdata() const;
    };

    template <typename T>
    struct MatrixTraits<TridiagonalMatrix<T>>
    {
        MAFOX_INHERIT_TRAITS(BandMatrix<T>);
    };
}

#endif // MAFOX_TRIDIAGONALMATRIX_H

namespace mafox
{
    template <typename T>
    TridiagonalMatrix<T>::TridiagonalMatrix(std::size_t size)
    : BandMatrix<T>(size, 1, 1)
    {}

    template <typename T>
    typename TridiagonalMatrix<T>::pointer TridiagonalMatrix<T>::lower_diagonal_data()
    {
        return BandMatrix<T>::lower_diagonal_data(0);
    }

    template <typename T>
    typename TridiagonalMatrix<T>::const_pointer TridiagonalMatrix<T>::lower_diagonal_cdata() const
    {
        return BandMatrix<T>::lower_diagonal_cdata(0);
    }

    template <typename T>
    typename TridiagonalMatrix<T>::pointer TridiagonalMatrix<T>::upper_diagonal_data()
    {
        return BandMatrix<T>::upper_diagonal_data(0);
    }

    template <typename T>
    typename TridiagonalMatrix<T>::const_pointer TridiagonalMatrix<T>::upper_diagonal_cdata() const
    {
        return BandMatrix<T>::upper_diagonal_cdata(0);
    }
}

#endif // MAFOX_TRIDIAGONALMATRIX_INC


#ifndef MAFOX_FDMMATRIX_INC
#define MAFOX_FDMMATRIX_INC


#ifndef MAFOX_FDMMATRIX_H
#define MAFOX_FDMMATRIX_H



namespace mafox
{
    namespace detail
    {
        enum class FDMCoeff
        {
            A, B, C
        };

        template <FDMCoeff COEFF, typename Callable>
        struct FDMCoeffGenerator
        {
            static constexpr FDMCoeff COEFFICIENT = COEFF;

            FDMCoeffGenerator(const Callable &callable);

            template <typename... Args>
            auto operator()(const Args &... args);

            template <typename... Args>
            auto operator()(const Args &... args) const;

            Callable generate;
        };

        template <typename T, typename... Generators>
        class FDMMatrixBuilder;

        template <bool HAS_A, typename T, typename... Generators>
        class FDMMatrixBuilderACoeff
        {
            using SelfType = FDMMatrixBuilder<T, Generators...>;
        public:
            template <typename Callable>
            auto a_coeff(const Callable &)
                -> FDMMatrixBuilder<T, Generators..., FDMCoeffGenerator<FDMCoeff::A, Callable>>;
        };

        template <typename T, typename... Generators>
        class FDMMatrixBuilderACoeff<true, T, Generators...>
        {};

        template <bool HAS_B, typename T, typename... Generators>
        class FDMMatrixBuilderBCoeff
        {
            using SelfType = FDMMatrixBuilder<T, Generators...>;
        public:
            template <typename Callable>
            auto b_coeff(const Callable &)
                -> FDMMatrixBuilder<T, Generators..., FDMCoeffGenerator<FDMCoeff::B, Callable>>;
        };

        template <typename T, typename... Generators>
        class FDMMatrixBuilderBCoeff<true, T, Generators...>
        {};

        template <bool HAS_C, typename T, typename... Generators>
        class FDMMatrixBuilderCCoeff
        {
            using SelfType = FDMMatrixBuilder<T, Generators...>;
        public:
            template <typename Callable>
            auto c_coeff(const Callable &)
                -> FDMMatrixBuilder<T, Generators..., FDMCoeffGenerator<FDMCoeff::C, Callable>>;
        };

        template <typename T, typename... Generators>
        class FDMMatrixBuilderCCoeff<true, T, Generators...>
        {};

        template <bool IS_COMPLETED, typename Builder>
        class FDMMatrixBuilderComputeMethod
        {
        public:
            auto compute();
        };

        template <typename Builder>
        class FDMMatrixBuilderComputeMethod<false, Builder>
        {};

        template <FDMCoeff COEFF, typename... Generators>
        constexpr bool has_coeff()
        {
            return (false || ... || (Generators::COEFFICIENT == COEFF));
        }

        template <typename... Generators>
        constexpr bool is_completed()
        {
            return has_coeff<FDMCoeff::A, Generators...>()
                && has_coeff<FDMCoeff::B, Generators...>()
                && has_coeff<FDMCoeff::C, Generators...>();
        }

        template <typename T, typename... Generators>
        class FDMMatrixBuilder :
            public FDMMatrixBuilderACoeff<has_coeff<FDMCoeff::A, Generators...>(), T, Generators...>,
            public FDMMatrixBuilderBCoeff<has_coeff<FDMCoeff::B, Generators...>(), T, Generators...>,
            public FDMMatrixBuilderCCoeff<has_coeff<FDMCoeff::C, Generators...>(), T, Generators...>,
            public FDMMatrixBuilderComputeMethod<is_completed<Generators...>(), FDMMatrixBuilder<T, Generators...>>
        {
        public:
            using value_type = T;

            FDMMatrixBuilder(const T &begin, const T &end, const T &step, std::tuple<Generators...> &&generators)
            : begin(begin), end(end), step(step), generators(std::forward<std::tuple<Generators...>>(generators))
            {}

        private:
            friend class FDMMatrixBuilderACoeff<has_coeff<FDMCoeff::A, Generators...>(), T, Generators...>;
            friend class FDMMatrixBuilderBCoeff<has_coeff<FDMCoeff::B, Generators...>(), T, Generators...>;
            friend class FDMMatrixBuilderCCoeff<has_coeff<FDMCoeff::C, Generators...>(), T, Generators...>;
            friend class FDMMatrixBuilderComputeMethod<is_completed<Generators...>(), FDMMatrixBuilder<T, Generators...>>;

            T begin, end, step;
            std::tuple<Generators...> generators;
        };
    }

    template <typename T>
    class FDMMatrixBorders
    {
    public:
        FDMMatrixBorders(const T &begin, const T &end, const T &step)
        : begin(begin), end(end), step(step)
        {}

        FDMMatrixBorders(const FDMMatrixBorders &) = default;

        FDMMatrixBorders(FDMMatrixBorders &&) = default;

        ~FDMMatrixBorders() = default;

        FDMMatrixBorders &operator=(const FDMMatrixBorders &) = default;

        FDMMatrixBorders &operator=(FDMMatrixBorders &&) = default;

        template <typename Callable>
        auto a_coeff(const Callable &) const
            -> detail::FDMMatrixBuilder
            <
                T,
                detail::FDMCoeffGenerator<detail::FDMCoeff::A, Callable>
            >;

        template <typename Callable>
        auto b_coeff(const Callable &) const
            -> detail::FDMMatrixBuilder
            <
                T,
                detail::FDMCoeffGenerator<detail::FDMCoeff::B, Callable>
            >;

        template <typename Callable>
        auto c_coeff(const Callable &) const
            -> detail::FDMMatrixBuilder
            <
                T,
                detail::FDMCoeffGenerator<detail::FDMCoeff::C, Callable>
            >;

    private:
        T begin, end, step;
    };


    template <typename T>
    FDMMatrixBorders<T> fdm_matrix(const T &start, const T &end, const T &step);
}

#endif // MAFOX_FDMMATRIX_H

namespace mafox
{
    namespace detail
    {
        template <FDMCoeff COEFF>
        struct IsCoeff
        {
            template <typename T>
            struct Checker
            {
                static constexpr bool value = (T::COEFFICIENT == COEFF);
            };
        };

        template <FDMCoeff COEFF, typename Callable>
        FDMCoeffGenerator<COEFF, Callable>::FDMCoeffGenerator(const Callable &callable)
        : generate(callable)
        {}

        template <FDMCoeff COEFF, typename Callable>
        template <typename... Args>
        auto FDMCoeffGenerator<COEFF, Callable>::operator()(const Args &... args)
        {
            return generate(args...);
        }

        template <FDMCoeff COEFF, typename Callable>
        template <typename... Args>
        auto FDMCoeffGenerator<COEFF, Callable>::operator()(const Args &... args) const
        {
            return generate(args...);
        }

        template <bool HAS_A, typename T, typename... Generators>
        template <typename Callable>
        auto FDMMatrixBuilderACoeff<HAS_A, T, Generators...>::a_coeff(const Callable &generator)
            -> FDMMatrixBuilder<T, Generators..., FDMCoeffGenerator<FDMCoeff::A, Callable>>
        {
            auto *self = reinterpret_cast<SelfType *>(this);

            return FDMMatrixBuilder<T, Generators..., FDMCoeffGenerator<FDMCoeff::A, Callable>>
            (
                self->begin,
                self->end,
                self->step,
                std::tuple_cat
                (
                    self->generators, 
                    std::make_tuple(FDMCoeffGenerator<FDMCoeff::A, Callable>(generator))
                )
            );
        }

        template <bool HAS_B, typename T, typename... Generators>
        template <typename Callable>
        auto FDMMatrixBuilderBCoeff<HAS_B, T, Generators...>::b_coeff(const Callable &generator)
            -> FDMMatrixBuilder<T, Generators..., FDMCoeffGenerator<FDMCoeff::B, Callable>>
        {
            auto *self = reinterpret_cast<SelfType *>(this);

            return FDMMatrixBuilder<T, Generators..., FDMCoeffGenerator<FDMCoeff::B, Callable>>
            (
                self->begin,
                self->end,
                self->step,
                std::tuple_cat
                (
                    self->generators, 
                    std::make_tuple(FDMCoeffGenerator<FDMCoeff::B, Callable>(generator))
                )
            );
        }

        template <bool HAS_C, typename T, typename... Generators>
        template <typename Callable>
        auto FDMMatrixBuilderCCoeff<HAS_C, T, Generators...>::c_coeff(const Callable &generator)
            -> FDMMatrixBuilder<T, Generators..., FDMCoeffGenerator<FDMCoeff::C, Callable>>
        {
            auto *self = reinterpret_cast<SelfType *>(this);

            return FDMMatrixBuilder<T, Generators..., FDMCoeffGenerator<FDMCoeff::C, Callable>>
            (
                self->begin,
                self->end,
                self->step,
                std::tuple_cat
                (
                    self->generators, 
                    std::make_tuple(FDMCoeffGenerator<FDMCoeff::C, Callable>(generator))
                )
            );
        }

        template <bool IS_COMPLETED, typename Builder>
        auto FDMMatrixBuilderComputeMethod<IS_COMPLETED, Builder>::compute()
        {
            auto *self = static_cast<Builder*>(this);

            using Generators = decltype(self->generators);
            using T = typename Builder::value_type;
            using FindA = metaxxa::Find<Generators, IsCoeff<FDMCoeff::A>::template Checker>;
            using FindB = metaxxa::Find<Generators, IsCoeff<FDMCoeff::B>::template Checker>;
            using FindC = metaxxa::Find<Generators, IsCoeff<FDMCoeff::C>::template Checker>;

            constexpr std::size_t A_INDEX = FindA::INDEX;
            constexpr std::size_t B_INDEX = FindB::INDEX;
            constexpr std::size_t C_INDEX = FindC::INDEX;

            auto &gen_a = std::get<A_INDEX>(self->generators);
            auto &gen_b = std::get<B_INDEX>(self->generators);
            auto &gen_c = std::get<C_INDEX>(self->generators);

            using R = std::common_type_t
            <
                std::invoke_result_t<typename FindA::Type, T>,
                std::invoke_result_t<typename FindB::Type, T>,
                std::invoke_result_t<typename FindC::Type, T>
            >;

            std::size_t result_size = static_cast<std::size_t>((self->end - self->begin) / self->step);

            if(result_size < 3)
                MAFOX_FATAL("Unable to create a finite difference matrix with less than 3 cells");

            TridiagonalMatrix<R> result(result_size);

            T x = self->begin;
            T h = self->step;

            auto *main_diagonal = result.diagonal_data();
            auto *upper_diagonal = result.upper_diagonal_data();
            auto *lower_diagonal = result.lower_diagonal_data();

            upper_diagonal[0] = gen_c(x);
            main_diagonal[0] = gen_b(x);

            x += h;
            --result_size;

            std::size_t i = 1;
            for(; i < result_size; ++i, x += h)
            {
                upper_diagonal[i]   = gen_c(x);
                main_diagonal[i]    = gen_b(x);
                lower_diagonal[i-1] = gen_a(x);
            }

            main_diagonal[i]    = gen_b(x);
            lower_diagonal[i-1] = gen_a(x);

            return result;
        }
    }

    template <typename T>
    template <typename Callable>
    auto FDMMatrixBorders<T>::a_coeff(const Callable &gen) const
        -> detail::FDMMatrixBuilder
            <
                T,
                detail::FDMCoeffGenerator<detail::FDMCoeff::A, Callable>
            >
    {
        return detail::FDMMatrixBuilder
        <
            T,
            detail::FDMCoeffGenerator<detail::FDMCoeff::A, Callable>
        >(begin, end, step, std::make_tuple(detail::FDMCoeffGenerator<detail::FDMCoeff::A, Callable>(gen)));
    }

    template <typename T>
    template <typename Callable>
    auto FDMMatrixBorders<T>::b_coeff(const Callable &gen) const
        -> detail::FDMMatrixBuilder
            <
                T,
                detail::FDMCoeffGenerator<detail::FDMCoeff::B, Callable>
            >
    {
        return detail::FDMMatrixBuilder
        <
            T,
            detail::FDMCoeffGenerator<detail::FDMCoeff::B, Callable>
        >(begin, end, step, std::make_tuple(detail::FDMCoeffGenerator<detail::FDMCoeff::B, Callable>(gen)));
    }

    template <typename T>
    template <typename Callable>
    auto FDMMatrixBorders<T>::c_coeff(const Callable &gen) const
        -> detail::FDMMatrixBuilder
            <
                T,
                detail::FDMCoeffGenerator<detail::FDMCoeff::C, Callable>
            >
    {
        return detail::FDMMatrixBuilder
        <
            T,
            detail::FDMCoeffGenerator<detail::FDMCoeff::C, Callable>
        >(begin, end, step, std::make_tuple(detail::FDMCoeffGenerator<detail::FDMCoeff::C, Callable>(gen)));
    }

    template <typename T>
    FDMMatrixBorders<T> fdm_matrix(const T &start, const T &end, const T &step)
    {
        return FDMMatrixBorders<T>(start, end, step);
    }
}

#endif // MAFOX_FDMMATRIX_INC

#endif // MAFOX_H


#endif // MAFOX_HPP