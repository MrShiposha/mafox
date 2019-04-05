#include "tests.h"

TEST_CASE("matrix creation", "[mafox::GMatrix]")
{
    SECTION("Matrix 1x1")
    {
        GMatrix<int> m1x1(1, 1, 4);

        REQUIRE(m1x1.rows() == 1);
        REQUIRE(m1x1.cols() == 1);
        REQUIRE(m1x1.at(0, 0) == 4);
    }

    SECTION("Matrix 2x3")
    {
        GMatrix<int> m2x3(2, 3);

        REQUIRE(m2x3.rows() == 2);
        REQUIRE(m2x3.cols() == 3);

        REQUIRE(m2x3.at(0, 0) == 0);
        REQUIRE(m2x3.at(0, 1) == 0);
        REQUIRE(m2x3.at(0, 2) == 0);

        REQUIRE(m2x3.at(1, 0) == 0);
        REQUIRE(m2x3.at(1, 1) == 0);
        REQUIRE(m2x3.at(1, 2) == 0);
    }
}

TEST_CASE("matrix assign", "[mafox::GMatrix]")
{
    SECTION("Matrix = Matrix")
    {
        GMatrix<int> m2x3(2, 3);
        GMatrix<int> r2x2(2, 2, 42);

        m2x3 = r2x2;

        REQUIRE(m2x3.rows() == 2);
        REQUIRE(m2x3.cols() == 2);

        REQUIRE(m2x3.at(0, 0) == 42);
        REQUIRE(m2x3.at(0, 1) == 42);

        REQUIRE(m2x3.at(1, 0) == 42);
        REQUIRE(m2x3.at(1, 1) == 42);
    }

    SECTION("set_at")
    {
        GMatrix<int> m2x3(2, 3);

        m2x3.set_at(0, 1, 555);

        REQUIRE(m2x3.rows() == 2);
        REQUIRE(m2x3.cols() == 3);

        REQUIRE(m2x3.at(0, 0) == 0);
        REQUIRE(m2x3.at(0, 1) == 555);
        REQUIRE(m2x3.at(0, 2) == 0);

        REQUIRE(m2x3.at(1, 0) == 0);
        REQUIRE(m2x3.at(1, 1) == 0);
        REQUIRE(m2x3.at(1, 2) == 0);
    }
}