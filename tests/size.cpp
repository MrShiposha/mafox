#include "tests.h"

TEST_CASE("Size2D")
{
    using Size = Size2D<double>;

    Size size = { 600.0, 800.0 };
    REQUIRE(size.width == 600.0);
    REQUIRE(size.height == 800.0);

    auto [w, h] = size;
    REQUIRE(w == 600.0);
    REQUIRE(h == 800.0);
}

TEST_CASE("Size3D")
{
    using Size = Size3D<double>;

    Size size = { 600.0, 800.0, 100.0 };
    REQUIRE(size.width == 600.0);
    REQUIRE(size.height == 800.0);
    REQUIRE(size.length == 100.0);

    auto [w, h, l] = size;
    REQUIRE(w == 600.0);
    REQUIRE(h == 800.0);
    REQUIRE(l == 100.0);
}