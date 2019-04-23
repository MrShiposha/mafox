#include "tests.h"

TEST_CASE("functiom creation", "[metaxxa::GridFunction]")
{
    SECTION("Empty")
    {
        GridFunction<double(double)> func;

        REQUIRE(func.nodes_count() == 0);
    }

    SECTION("List")
    {
        /* With GCC calling Tuples... constructor. Possible bug of GCC */
        GridFunction func = 
        {
            f(1.42, 0) = 3.14,
            f(1.55, 1) = 6.14
        };

        using F = decltype(func);

        static_assert(is_same_v<typename F::Result, double>);
        static_assert(is_same_v<typename F::template Argument<0>, double>);
        static_assert(is_same_v<typename F::template Argument<1>, int>);

        REQUIRE(func.nodes_count() == 2);
    }

    SECTION("General c-tor")
    {
        GridFunction func
        (
            f(1.42, 0) = 3.14,
            f(1.55, 1) = 6.14
        );

        // using F = decltype(func);

        // static_assert(is_same_v<typename F::Result, double>);
        // static_assert(is_same_v<typename F::template Argument<0>, double>);
        // static_assert(is_same_v<typename F::template Argument<1>, int>);

        // REQUIRE(func.nodes_count() == 2);
    }

    // SECTION("General c-tor std tuple")
    // {
    //     std::tuple<double, double, int> v1(3.14, 1.42, 0);
    //     std::tuple<double, double, int> v2(6.14, 1.55, 1);

    //     GridFunction func
    //     (
    //         v1,
    //         v2
    //     );

    //     using F = decltype(func);

    //     static_assert(is_same_v<typename F::Result, double>);
    //     static_assert(is_same_v<typename F::template Argument<0>, double>);
    //     static_assert(is_same_v<typename F::template Argument<1>, int>);

    //     REQUIRE(func.nodes_count() == 2);
    // }
    
    // TODO: getting node
}