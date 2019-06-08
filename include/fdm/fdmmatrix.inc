#ifndef MAFOX_FDMMATRIX_INC
#define MAFOX_FDMMATRIX_INC

#include "fdmmatrix.h"
#include "../tridiagonalmatrix.inc"
#include "../../lib/metaxxa/metaxxa.hpp"

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