#ifndef MAFOX_FDMMATRIX_H
#define MAFOX_FDMMATRIX_H

#include <type_traits>
#include <tuple>

#include "../tridiagonalmatrix.h"

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