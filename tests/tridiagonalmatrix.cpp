#include "tests.h"

TEST_CASE("[mafox::TridiagonalMatrix]")
{
    TridiagonalMatrix<int> m(3);
    m.set_element(0, 0, 1);
    m.set_element(0, 1, 10);

    m.set_element(1, 0, 2);
    m.set_element(1, 1, 20);
    m.set_element(1, 2, 200);

    m.set_element(2, 1, 30);
    m.set_element(2, 2, 300);

    SECTION("transposed")
    {
        TridiagonalMatrix<int> mtr = m.transposed();
        REQUIRE(mtr(0, 0) == 1);
        REQUIRE(mtr(0, 1) == 2);

        REQUIRE(mtr(1, 0) == 10);
        REQUIRE(mtr(1, 1) == 20);
        REQUIRE(mtr(1, 2) == 30);

        REQUIRE(mtr(2, 1) == 200);
        REQUIRE(mtr(2, 2) == 300);
    }

    SECTION("transposed_rsd")
    {
        TridiagonalMatrix<int> mtr = m.transposed_rsd();

        REQUIRE(mtr(0, 0) == 300);
        REQUIRE(mtr(0, 1) == 200);

        REQUIRE(mtr(1, 0) == 30);
        REQUIRE(mtr(1, 1) == 20);
        REQUIRE(mtr(1, 2) == 10);

        REQUIRE(mtr(2, 1) == 2);
        REQUIRE(mtr(2, 2) == 1);
    }

    SECTION("lower/upper bandwidth")
    {
        REQUIRE(m.lower_bandwidth() == 1);
        REQUIRE(m.upper_bandwidth() == 1);
    }

    SECTION("diagonal data")
    {
        REQUIRE(m.diagonal_data()[0] == 1);
        REQUIRE(m.diagonal_data()[1] == 20);
        REQUIRE(m.diagonal_data()[2] == 300);

        REQUIRE(m.upper_diagonal_data()[0] == 10);
        REQUIRE(m.upper_diagonal_data()[1] == 200);

        REQUIRE(m.lower_diagonal_data()[0] == 2);
        REQUIRE(m.lower_diagonal_data()[1] == 30);
    }
}