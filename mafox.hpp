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
#include <tuple>
#include <initializer_list>

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

        static constexpr Type VALUE = LITERAL;
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
        using Tail = LiteralList<typename LiteralH::Type, LiteralTail::VALUE...>;
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
            return Head::VALUE;
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


namespace metaxxa
{
    template <typename... Args>
    class TypeTuple;

    namespace detail
    {
        template <typename... Args>
        struct TupleConcatenator
        {
            template <typename... RHSArgs>
            static constexpr auto result_tuple(TypeTuple<RHSArgs...> &&) 
                -> TypeTuple<Args..., RHSArgs...>;
        };
    }

    template <typename... Args>
    class TypeTuple : public TypeList<Args...>
    {
    public:
        using List = metaxxa::TypeList<Args...>;

        template <std::size_t INDEX>
        using Get = typename std::tuple_element_t<INDEX, List>;

        template <typename RHSTuple>
        using Concat = decltype(detail::TupleConcatenator<Args...>::template result_tuple(std::declval<RHSTuple>()));

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



#ifndef METAXXA_SIZECONSTANT_H
#define METAXXA_SIZECONSTANT_H



#ifndef METAXXA_UPPERVALUE_H
#define METAXXA_UPPERVALUE_H

namespace metaxxa
{
    template <typename T>
    struct UpperValue
    {
        using Type = typename T::value_type;

        static constexpr Type VALUE = T::value;
    };
}

#endif // METAXXA_UPPERVALUE_H

namespace metaxxa
{
    template <std::size_t INDEX>
    using SizeConstant = UpperValue<std::integral_constant<std::size_t, INDEX>>;
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
        return ParametersCount<T>::VALUE;
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

#ifndef METAXXA_ALGORITHM_FIND_H
#define METAXXA_ALGORITHM_FIND_H


namespace metaxxa
{
    namespace detail
    {
        template
        <
            typename TupleT,
            template <typename T> typename Functor,
            std::size_t N   = 0,
            bool PREV_FOUND = false,
            bool ENOUGH     = (N >= std::tuple_size_v<TupleT>)
        >
        struct Find : Find
        <
            TupleT, 
            Functor,
            N + 1,
            Functor<std::tuple_element_t<N, TupleT>>::VALUE,
            N + 1 >= std::tuple_size_v<TupleT>
        >
        {};

        template
        <
            typename TupleT,
            template <typename T> typename Functor,
            std::size_t N,
            bool ENOUGH
        >
        struct Find
        <
            TupleT,
            Functor,
            N,
            true,
            ENOUGH
        >
        {
            static constexpr bool FOUND = true;
            static constexpr std::size_t INDEX = N - 1;

            using Type = std::tuple_element_t<INDEX, TupleT>;

            template <typename T>
            using TypeOr = Type;
        };
        
        template
        <
            typename TupleT,
            template <typename T> typename Functor,
            std::size_t N
        >
        struct Find
        <
            TupleT,
            Functor,
            N,
            false,
            true
        >
        {
            static constexpr bool FOUND = false;

            template <typename T>
            using TypeOr = T;
        };
    }

    template 
    <
        typename TupleT,
        template <typename T> typename Functor
    >
    using Find = detail::Find<TupleT, Functor>;
}

#endif // METAXXA_ALGORITHM_FIND_H

#endif // METAXXA_ALGORITHM_H


#ifndef METAXXA_ENABLEFNIF_H
#define METAXXA_ENABLEFNIF_H


#define ENABLE_T_IF(CONDITION) typename = std::enable_if_t<CONDITION>

#define ENABLE_FN_IF_T(CONDITION) std::enable_if_t<CONDITION> *

#define ENABLE_FN_IF(CONDITION) ENABLE_FN_IF_T(CONDITION) = nullptr

#endif // METAXXA_ENABLEFNIF_H


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

#ifndef MAFOX_GRIDFUNCTION_INC
#define MAFOX_GRIDFUNCTION_INC


#ifndef MAFOX_GRIDFUNCTION_H
#define MAFOX_GRIDFUNCTION_H



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