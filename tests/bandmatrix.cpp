#include "tests.h"

TEST_CASE("creation/setting/getting", "[mafox::BandMatrix]")
{
    SECTION("l = u = 0")
    {
        BandMatrix<double> m(3, 0, 0);

        REQUIRE(m.rows() == m.cols());
        REQUIRE(m.rows() == 3);
        REQUIRE(m.cols() == 3);

        REQUIRE(m.lower_bandwidth() == 0);
        REQUIRE(m.upper_bandwidth() == 0);

        REQUIRE(m.try_set_element(0, 0, 1));
        REQUIRE(!m.try_set_element(0, 1, 10));
        REQUIRE(!m.try_set_element(0, 2, 100));

        REQUIRE(!m.try_set_element(1, 0, 2));
        REQUIRE(m.try_set_element(1, 1, 20));
        REQUIRE(!m.try_set_element(1, 2, 200));

        REQUIRE(!m.try_set_element(2, 0, 3));
        REQUIRE(!m.try_set_element(2, 1, 30));
        REQUIRE(m.try_set_element(2, 2, 300));

        REQUIRE(m(0, 0) == 1);
        REQUIRE(m(0, 1) == 0);
        REQUIRE(m(0, 2) == 0);

        REQUIRE(m(1, 0) == 0);
        REQUIRE(m(1, 1) == 20);
        REQUIRE(m(1, 2) == 0);

        REQUIRE(m(2, 0) == 0);
        REQUIRE(m(2, 1) == 0);
        REQUIRE(m(2, 2) == 300);
    }

    SECTION("l = 1, u = 0")
    {
        BandMatrix<double> m(3, 1, 0);

        REQUIRE(m.rows() == m.cols());
        REQUIRE(m.rows() == 3);
        REQUIRE(m.cols() == 3);

        REQUIRE(m.lower_bandwidth() == 1);
        REQUIRE(m.upper_bandwidth() == 0);

        REQUIRE(m.try_set_element(0, 0, 1));
        REQUIRE(!m.try_set_element(0, 1, 10));
        REQUIRE(!m.try_set_element(0, 2, 100));

        REQUIRE(m.try_set_element(1, 0, 2));
        REQUIRE(m.try_set_element(1, 1, 20));
        REQUIRE(!m.try_set_element(1, 2, 200));

        REQUIRE(!m.try_set_element(2, 0, 3));
        REQUIRE(m.try_set_element(2, 1, 30));
        REQUIRE(m.try_set_element(2, 2, 300));

        REQUIRE(m(0, 0) == 1);
        REQUIRE(m(0, 1) == 0);
        REQUIRE(m(0, 2) == 0);

        REQUIRE(m(1, 0) == 2);
        REQUIRE(m(1, 1) == 20);
        REQUIRE(m(1, 2) == 0);

        REQUIRE(m(2, 0) == 0);
        REQUIRE(m(2, 1) == 30);
        REQUIRE(m(2, 2) == 300);
    }

    SECTION("l = 0, u = 1")
    {
        BandMatrix<double> m(3, 0, 1);

        REQUIRE(m.rows() == m.cols());
        REQUIRE(m.rows() == 3);
        REQUIRE(m.cols() == 3);

        REQUIRE(m.lower_bandwidth() == 0);
        REQUIRE(m.upper_bandwidth() == 1);

        REQUIRE(m.try_set_element(0, 0, 1));
        REQUIRE(m.try_set_element(0, 1, 10));
        REQUIRE(!m.try_set_element(0, 2, 100));

        REQUIRE(!m.try_set_element(1, 0, 2));
        REQUIRE(m.try_set_element(1, 1, 20));
        REQUIRE(m.try_set_element(1, 2, 200));

        REQUIRE(!m.try_set_element(2, 0, 3));
        REQUIRE(!m.try_set_element(2, 1, 30));
        REQUIRE(m.try_set_element(2, 2, 300));

        REQUIRE(m(0, 0) == 1);
        REQUIRE(m(0, 1) == 10);
        REQUIRE(m(0, 2) == 0);

        REQUIRE(m(1, 0) == 0);
        REQUIRE(m(1, 1) == 20);
        REQUIRE(m(1, 2) == 200);

        REQUIRE(m(2, 0) == 0);
        REQUIRE(m(2, 1) == 0);
        REQUIRE(m(2, 2) == 300);
    }

    SECTION("l = u = 1")
    {
        BandMatrix<double> m(3, 1, 1);

        REQUIRE(m.rows() == m.cols());
        REQUIRE(m.rows() == 3);
        REQUIRE(m.cols() == 3);

        REQUIRE(m.lower_bandwidth() == 1);
        REQUIRE(m.upper_bandwidth() == 1);

        REQUIRE(m.try_set_element(0, 0, 1));
        REQUIRE(m.try_set_element(0, 1, 10));
        REQUIRE(!m.try_set_element(0, 2, 100));

        REQUIRE(m.try_set_element(1, 0, 2));
        REQUIRE(m.try_set_element(1, 1, 20));
        REQUIRE(m.try_set_element(1, 2, 200));

        REQUIRE(!m.try_set_element(2, 0, 3));
        REQUIRE(m.try_set_element(2, 1, 30));
        REQUIRE(m.try_set_element(2, 2, 300));

        REQUIRE(m(0, 0) == 1);
        REQUIRE(m(0, 1) == 10);
        REQUIRE(m(0, 2) == 0);

        REQUIRE(m(1, 0) == 2);
        REQUIRE(m(1, 1) == 20);
        REQUIRE(m(1, 2) == 200);

        REQUIRE(m(2, 0) == 0);
        REQUIRE(m(2, 1) == 30);
        REQUIRE(m(2, 2) == 300);
    }
}