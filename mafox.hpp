#ifndef MAFOX_HPP
#define MAFOX_HPP

#include <type_traits>
#include <utility>
#include <cstring>
#include <cstddef>
#include <cstdint>
#include <memory>
#include <vector>
#include <cmath>

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

namespace mafox
{
    using Byte = unsigned char;
}

#endif // MAFOX_DETAIL_DEF_H

#ifndef MAFOX_GMATRIX_INC
#define MAFOX_GMATRIX_INC


#ifndef MAFOX_GMATRIX_H
#define MAFOX_GMATRIX_H



#ifndef MAFOX_MATRIXALLOCATOR_H
#define MAFOX_MATRIXALLOCATOR_H


namespace mafox
{
    template <typename T>
    struct MatrixAllocator
    {
        using Type = T;
        using Traits = std::allocator_traits<MatrixAllocator<T>>;

        using value_type      = Type;
        using size_type       = typename std::allocator<T>::size_type;
        using difference_type = typename std::allocator<T>::difference_type;
        using pointer         = typename std::allocator<T>::pointer;
        using const_pointer   = typename std::allocator<T>::const_pointer;
        using reference       = typename std::allocator<T>::reference;
        using const_reference = typename std::allocator<T>::const_reference;

        template<class U> struct rebind 
        {
            using other = MatrixAllocator<U>; 
        };

        template<class U, class... Args> 
        void construct(U* p, Args&&... args);
        
        template<class U> void destroy(U* p);

        MatrixAllocator() = default;

        template <typename U>
        MatrixAllocator(const MatrixAllocator<U> &);

        T *allocate(std::size_t);

        T *reallocate(T *, std::size_t);

        void deallocate(T *, std::size_t);
    };
}

template <typename T, typename U>
bool operator==(const mafox::MatrixAllocator<T> &, const mafox::MatrixAllocator<U> &);

template <typename T, typename U>
bool operator!=(const mafox::MatrixAllocator<T> &, const mafox::MatrixAllocator<U> &);

#endif // MAFOX_MATRIXALLOCATOR_H


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
        typename Allocator = MatrixAllocator<T>
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

#undef MAFOX_GMATRIX
#undef MAFOX_SELF

#endif // MAFOX_GMATRIX_INC

#ifndef MAFOX_LEGENDRE_H
#define MAFOX_LEGENDRE_H



#ifndef MAFOX_LEGENDREСACHEMANAGER_H
#define MAFOX_LEGENDREСACHEMANAGER_H

namespace mafox
{
    struct DefaultLegendreCache;

    // Legendre Сoefficient Cache Manager
    // You can declare you own specialization for type T
    // (e.g. using Type = MyLCacheClass)
    // 
    // Let 
    //      BoolT = any type, that can be casted to bool
    //      IntT = see legendre.h
    //      
    //      n = power of legendre polynomial
    //      alpha = (2*n - 1)/n
    //      beta  = (n-1)/n
    // MyLCacheClass must have following members:
    //      * BoolT is_in_cache(IntT power) const // is coefficients for power in cache?
    //
    //      * void store(IntT power, double alpha, double beta) // store power, alpha and beta in cache 
    //
    //      * double alpha(IntT power) const
    //
    //      * double beta(IntT power) const
    template <typename T>
    struct LegendreCacheManager
    {
        using Type = DefaultLegendreCache;
    };
}

#endif // MAFOX_LEGENDREСACHEMANAGER_H

#define ENABLE_IF_INT_POWER ENABLE_FN_IF(std::is_integral_v<IntT>)

namespace mafox
{
    namespace detail
    {
        class DefaultLegendreCache;
    }

    struct DefaultLegendreCache
    {
        mafox_inline DefaultLegendreCache();

        mafox_inline DefaultLegendreCache(const DefaultLegendreCache &);

        mafox_inline DefaultLegendreCache(DefaultLegendreCache &&);

        // Is power in cache?
        template <typename IntT>
        mafox_inline bool is_in_cache(IntT power, ENABLE_IF_INT_POWER) const noexcept;

        template <typename IntT>
        mafox_inline void store(IntT power, double alpha, double beta, ENABLE_IF_INT_POWER);

        // Returns (2*power - 1)/power
        template <typename IntT>
        mafox_inline double alpha(IntT power, ENABLE_IF_INT_POWER) const; 

        // Returns (power - 1)/power
        template <typename IntT>
        mafox_inline double beta(IntT power, ENABLE_IF_INT_POWER) const;

    private:
        std::unique_ptr<detail::DefaultLegendreCache> self;
    };

    template <typename T, typename IntT>
    mafox_inline auto legendre_polynomial
    (
        metaxxa::TypeOrRef<const T> x,
        IntT power, 
        ENABLE_IF_INT_POWER
    );

    template <typename T, typename IntT, typename Cache = typename LegendreCacheManager<T>::Type>
    mafox_inline auto legendre_polynomial
    (
        metaxxa::TypeOrRef<const T> x,
        IntT power,
        Cache &,
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

    // Returns std::pair of P_n(x) P_{n-1}(x)
    template <typename T, typename IntT, typename Cache = typename LegendreCacheManager<T>::Type>
    auto legendre_polynomial_pair
    (
        metaxxa::TypeOrRef<const T> x,
        IntT power,
        Cache &,
        ENABLE_IF_INT_POWER
    );

    // LP means Legendre Polynomial
    // In LaTeX:
    //      $$ LP_n1 is P_{n-1}(x) $$
    //      $$ LP_n2 is P_{n-2}(x) $$
    template 
    <
        typename T, 
        typename IntT, 
        typename LP_n1, 
        typename LP_n2, 
        typename Cache = typename LegendreCacheManager<T>::Type
    >
    mafox_inline auto legendre_polynomial_next
    (
        metaxxa::TypeOrRef<const T> x,
        IntT power,
        metaxxa::TypeOrRef<const LP_n1> lp_n1,
        metaxxa::TypeOrRef<const LP_n2> lp_n2,
        Cache &,
        ENABLE_IF_INT_POWER
    );

    // LP means Legendre Polynomial
    // In LaTeX:
    //      $$ LP_n is P_n(x) $$
    //      $$ LP_n1 is P_{n-1}(x) $$
    template <typename T, typename IntT, typename LP_n, typename LP_n1>
    mafox_inline auto legendre_polynomial_derivative
    (
        metaxxa::TypeOrRef<const T> x,
        IntT power,
        metaxxa::TypeOrRef<LP_n> lp_n,
        metaxxa::TypeOrRef<const LP_n1> lp_n1,
        ENABLE_IF_INT_POWER
    );

    template 
    <
        typename T = double, 
        typename IntT = int, 
        typename Cache = typename LegendreCacheManager<T>::Type
    >
    class LegendrePolynomial
    {
    public:
        mafox_inline LegendrePolynomial();

        mafox_inline LegendrePolynomial(const Cache &cache);

        mafox_inline LegendrePolynomial(Cache &&cache);

        mafox_inline LegendrePolynomial(IntT power);

        mafox_inline LegendrePolynomial(IntT power, const Cache &cache);

        mafox_inline LegendrePolynomial(IntT power, Cache &&cache);

        mafox_inline LegendrePolynomial &power(IntT p);

        mafox_inline IntT power() const;

        mafox_inline LegendrePolynomial &next_power();

        mafox_inline auto operator()(metaxxa::TypeOrRef<const T> x);

        mafox_inline auto derivative(metaxxa::TypeOrRef<const T> x);

        mafox_inline auto derivative
        (
            metaxxa::TypeOrRef<const T> x,
            metaxxa::TypeOrRef<const T> lp_n,
            metaxxa::TypeOrRef<const T> lp_n1
        ) const;

        template <typename LP_n, typename LP_n1>
        mafox_inline auto derivative
        (
            metaxxa::TypeOrRef<const T> x,
            metaxxa::TypeOrRef<const LP_n> lp_n,
            metaxxa::TypeOrRef<const LP_n1> lp_n1
        ) const;

    private:
        IntT _power;
        Cache cache;
    };
}

#undef ENABLE_IF_INT_POWER

#endif // MAFOX_LEGENDRE_H


#define ENABLE_IF_INT_POWER ENABLE_FN_IF_T(std::is_integral_v<IntT>)

#define MAFOX_SELF LegendrePolynomial<T, IntT, Cache>
    
#define MAFOX_LP(ReturnType) \
    template <typename T, typename IntT, typename Cache> \
    ReturnType MAFOX_EXPAND(MAFOX_SELF)

#define INLINE_MAFOX_LP(ReturnType) \
    template <typename T, typename IntT, typename Cache> \
    MAFOX_EXPAND(mafox_inline) ReturnType MAFOX_EXPAND(MAFOX_SELF)

namespace mafox
{
    namespace detail
    {
        class DefaultLegendreCache
        {
        public:
            using Alpha = double;
            using Beta  = double;

            std::vector<std::pair<Alpha, Beta>> cache;
        };

        template <typename T>
        static inline typename LegendreCacheManager<T>::Type default_cache;
    }

    mafox_inline DefaultLegendreCache::DefaultLegendreCache()
    : self(new detail::DefaultLegendreCache())
    {
        self->cache.push_back(std::make_pair(std::nan("Legendre alpha for n == 0"), std::nan("Legendre beta for n == 0")));
        self->cache.push_back(std::make_pair(1., 0.));
    }

    mafox_inline DefaultLegendreCache::DefaultLegendreCache(const DefaultLegendreCache &other)
    : self(new detail::DefaultLegendreCache())
    {
        self->cache = other.self->cache;
    }

    mafox_inline DefaultLegendreCache::DefaultLegendreCache(DefaultLegendreCache &&other)
    : self(std::move(other.self))
    {}

    template <typename IntT>
    mafox_inline bool DefaultLegendreCache::is_in_cache(IntT power, ENABLE_IF_INT_POWER) const noexcept
    {
        return self->cache.size() > power;
    }

    template <typename IntT>
    mafox_inline void DefaultLegendreCache::store(IntT power, double alpha, double beta, ENABLE_IF_INT_POWER)
    {
        if(is_in_cache(power))
            self->cache[power] = std::make_pair(alpha, beta);
        else
        {
            assert(power == self->cache.size() && "INTERNAL ERROR: DefaultLegendreCache can't store nonlinear");

            self->cache.push_back(std::make_pair(alpha, beta));
        }
    }

    template <typename IntT>
    mafox_inline double DefaultLegendreCache::alpha(IntT power, ENABLE_IF_INT_POWER) const
    {
        assert(is_in_cache(power));

        return self->cache[power].first;
    }

    template <typename IntT>
    mafox_inline double DefaultLegendreCache::beta(IntT power, ENABLE_IF_INT_POWER) const
    {
        assert(is_in_cache(power));

        return self->cache[power].second;
    }

    template <typename T, typename IntT>
    mafox_inline auto legendre_polynomial
    (
        metaxxa::TypeOrRef<const T> x,
        IntT power, 
        ENABLE_IF_INT_POWER
    )
    {
        return legendre_polynomial<T, IntT>(x, power, detail::default_cache<T>);
    }

    template <typename T, typename IntT, typename Cache>
    mafox_inline auto legendre_polynomial
    (
        metaxxa::TypeOrRef<const T> x,
        IntT power, 
        Cache &cache,
        ENABLE_IF_INT_POWER
    )
    {
        return legendre_polynomial_pair<T, IntT>(x, power, cache).first;
    }

    template <typename T, typename IntT>
    auto legendre_polynomial_pair
    (
        metaxxa::TypeOrRef<const T> x,
        IntT power, 
        ENABLE_IF_INT_POWER
    )
    {
        return legendre_polynomial_pair<T, IntT>(x, power, detail::default_cache<T>);
    }

    template <typename T, typename IntT, typename Cache>
    auto legendre_polynomial_pair
    (
        metaxxa::TypeOrRef<const T> x,
        IntT power,
        Cache &cache,
        ENABLE_IF_INT_POWER
    )
    {
        assert(power >= 0);
        
        if(power == 0)
            return std::pair<T, T>(1.0, std::nan("Result of Legendre polynomial of power -1"));
        else if(power == 1)
            return std::pair<T, T>(x, 1.0);

        // Start from power (n) == 2
        T pl_n1     = x;   // P_{n-1}(x) (now P_1(x))
        T pl_n2     = 1.0; // P_{n-2}(x) (now P_0(x))
        T old_pl_n1 = 0.0; // 0.0 is for init value

        // i <= power: for pl_n1 == P_power at end of cycle
        for(IntT i = 2; i <= power; ++i)
        {
            old_pl_n1 = pl_n1;
            pl_n1 = legendre_polynomial_next<T, IntT, T, T>(x, i, pl_n1, pl_n2, cache);
            pl_n2 = old_pl_n1;
        }

        return std::make_pair(pl_n1, pl_n2);
    }

    template <typename T, typename IntT, typename LP_n1, typename LP_n2, typename Cache>
    mafox_inline auto legendre_polynomial_next
    (
        metaxxa::TypeOrRef<const T> x,
        IntT power,
        metaxxa::TypeOrRef<const LP_n1> lp_n1,
        metaxxa::TypeOrRef<const LP_n2> lp_n2,
        Cache &cache,
        ENABLE_IF_INT_POWER
    )
    {
        assert(power >= 0);

        if(!cache.is_in_cache(power))
            cache.store(power, (2*power - 1)/static_cast<T>(power), (power - 1)/static_cast<T>(power));
        
        return cache.alpha(power) * x * lp_n1 - cache.beta(power)*lp_n2;
    }

    template <typename T, typename IntT, typename LP_n, typename LP_n1>
    mafox_inline auto legendre_polynomial_derivative
    (
        metaxxa::TypeOrRef<const T> x,
        IntT power,
        metaxxa::TypeOrRef<LP_n> lp_n,
        metaxxa::TypeOrRef<const LP_n1> lp_n1,
        ENABLE_IF_INT_POWER
    )
    {
        assert(power >= 0);

        if(power == 0)
            return T(0);

        const T k = power/(1 - x*x);
        return k*(lp_n1 - x * lp_n);
    }

    INLINE_MAFOX_LP()::LegendrePolynomial()
    : LegendrePolynomial(0)
    {}

    INLINE_MAFOX_LP()::LegendrePolynomial(const Cache &cache)
    : LegendrePolynomial(0, cache)
    {}

    INLINE_MAFOX_LP()::LegendrePolynomial(Cache &&cache)
    : LegendrePolynomial(0, std::move(cache))
    {}

    INLINE_MAFOX_LP()::LegendrePolynomial(IntT power)
    : _power(power), cache()
    {}

    INLINE_MAFOX_LP()::LegendrePolynomial(IntT power, const Cache &cache)
    : _power(power), cache(cache)
    {}

    INLINE_MAFOX_LP()::LegendrePolynomial(IntT power, Cache &&cache)
    : _power(power), cache(std::move(cache))
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

    INLINE_MAFOX_LP(auto)::operator()(metaxxa::TypeOrRef<const T> x)
    {
        return legendre_polynomial<T, IntT>(x, _power, cache);
    }

    INLINE_MAFOX_LP(auto)::derivative(metaxxa::TypeOrRef<const T> x)
    {
        if(_power == 0)
            return T(0);

        auto [p_n, p_n1] = legendre_polynomial_pair<T, IntT>(x, _power, cache);

        return derivative
        (
            x,
            p_n,
            p_n1
        );
    }

    INLINE_MAFOX_LP(auto)::derivative
    (
        metaxxa::TypeOrRef<const T> x,
        metaxxa::TypeOrRef<const T> lp_n,
        metaxxa::TypeOrRef<const T> lp_n1
    ) const
    {
        return derivative<T, T>(x, lp_n, lp_n1);
    }

    template <typename T, typename IntT, typename Cache>
    template <typename LP_n, typename LP_n1>
    mafox_inline auto LegendrePolynomial<T, IntT, Cache>::derivative
    (
        metaxxa::TypeOrRef<const T> x,
        metaxxa::TypeOrRef<const LP_n> lp_n,
        metaxxa::TypeOrRef<const LP_n1> lp_n1
    ) const
    {
        return legendre_polynomial_derivative<T, IntT, LP_n, LP_n1>(x, _power, lp_n, lp_n1);
    }
}

#undef INLINE_MAFOX_LP
#undef MAFOX_LP
#undef MAFOX_SELF

#undef ENABLE_IF_INT_POWER

#endif // MAFOX_H


#endif // MAFOX_HPP