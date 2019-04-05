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
        GridFunction<double(double, int)> func = 
        {
            f(1.42, 0) = 3.14,
            f(1.55, 1) = 6.14
        };

        REQUIRE(func.nodes_count() == 2);
        REQUIRE(func.node(0) == std::make_tuple(1.42, 0, 3.14));
        REQUIRE(func.node(1) == std::make_tuple(1.55, 1, 6.14));
    }
}