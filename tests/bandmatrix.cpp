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

TEST_CASE("transpose band matrix", "[mafox::BandMatrix]")
{
    SECTION("l = u = 0")
    {
        BandMatrix<double> m(3, 0, 0);

        m.set_element(0, 0, 1);
        m.set_element(1, 1, 20);
        m.set_element(2, 2, 300);

        m.transpose();

        REQUIRE(m(0, 0) == 1);
        REQUIRE(m(1, 1) == 20);
        REQUIRE(m(2, 2) == 300);
    }

    SECTION("l = 1, u = 0")
    {
        BandMatrix<double> m(3, 1, 0);

        m.set_element(0, 0, 1);
        m.set_element(1, 0, 2);
        m.set_element(1, 1, 20);
        m.set_element(2, 1, 30);
        m.set_element(2, 2, 300);

        m.transpose();

        REQUIRE(m.lower_bandwidth() == 0);
        REQUIRE(m.upper_bandwidth() == 1);

        REQUIRE(m(0, 0) == 1);
        REQUIRE(m(1, 1) == 20);
        REQUIRE(m(2, 2) == 300);

        REQUIRE(m(0, 1) == 2);
        REQUIRE(m(1, 2) == 30);
    }

    SECTION("l = 0, u = 1")
    {
        BandMatrix<double> m(3, 0, 1);

        m.set_element(0, 0, 1);
        m.set_element(0, 1, 10);
        m.set_element(1, 1, 20);
        m.set_element(1, 2, 200);
        m.set_element(2, 2, 300);

        m.transpose();

        REQUIRE(m.lower_bandwidth() == 1);
        REQUIRE(m.upper_bandwidth() == 0);

        REQUIRE(m(0, 0) == 1);
        REQUIRE(m(1, 1) == 20);
        REQUIRE(m(2, 2) == 300);

        REQUIRE(m(1, 0) == 10);
        REQUIRE(m(2, 1) == 200);
    }

    SECTION("l = u = 1")
    {
        BandMatrix<double> m(3, 1, 1);

        m.set_element(0, 0, 1);
        m.set_element(0, 1, 10);
        m.set_element(1, 0, 2);
        m.set_element(1, 1, 20);
        m.set_element(1, 2, 200);
        m.set_element(2, 1, 30);
        m.set_element(2, 2, 300);

        m.transpose();

        REQUIRE(m.lower_bandwidth() == 1);
        REQUIRE(m.upper_bandwidth() == 1);

        REQUIRE(m(0, 0) == 1);
        REQUIRE(m(1, 1) == 20);
        REQUIRE(m(2, 2) == 300);

        REQUIRE(m(0, 1) == 2);
        REQUIRE(m(1, 0) == 10);
        REQUIRE(m(1, 2) == 30);
        REQUIRE(m(2, 1) == 200);
    }
}

TEST_CASE("transpose_rsd band matrix", "[mafox::BandMatrix]")
{
    SECTION("l = u = 0")
    {
        BandMatrix<double> m(3, 0, 0);

        m.set_element(0, 0, 1);
        m.set_element(1, 1, 20);
        m.set_element(2, 2, 300);

        m.transpose_rsd();

        REQUIRE(m(0, 0) == 300);
        REQUIRE(m(1, 1) == 20);
        REQUIRE(m(2, 2) == 1);
    }

    SECTION("l = 1, u = 0")
    {
        BandMatrix<double> m(3, 1, 0);

        m.set_element(0, 0, 1);
        m.set_element(1, 0, 2);
        m.set_element(1, 1, 20);
        m.set_element(2, 1, 30);
        m.set_element(2, 2, 300);

        m.transpose_rsd();

        REQUIRE(m.lower_bandwidth() == 1);
        REQUIRE(m.upper_bandwidth() == 0);

        REQUIRE(m(0, 0) == 300);
        REQUIRE(m(1, 1) == 20);
        REQUIRE(m(2, 2) == 1);

        REQUIRE(m(1, 0) == 30);
        REQUIRE(m(2, 1) == 2);
    }

    SECTION("l = 0, u = 1")
    {
        BandMatrix<double> m(3, 0, 1);

        m.set_element(0, 0, 1);
        m.set_element(0, 1, 10);
        m.set_element(1, 1, 20);
        m.set_element(1, 2, 200);
        m.set_element(2, 2, 300);

        m.transpose_rsd();

        REQUIRE(m.lower_bandwidth() == 0);
        REQUIRE(m.upper_bandwidth() == 1);

        REQUIRE(m(0, 0) == 300);
        REQUIRE(m(1, 1) == 20);
        REQUIRE(m(2, 2) == 1);

        REQUIRE(m(0, 1) == 200);
        REQUIRE(m(1, 2) == 10);
    }

    SECTION("l = u = 1")
    {
        BandMatrix<double> m(3, 1, 1);

        m.set_element(0, 0, 1);
        m.set_element(0, 1, 10);
        m.set_element(1, 0, 2);
        m.set_element(1, 1, 20);
        m.set_element(1, 2, 200);
        m.set_element(2, 1, 30);
        m.set_element(2, 2, 300);

        m.transpose_rsd();

        REQUIRE(m.lower_bandwidth() == 1);
        REQUIRE(m.upper_bandwidth() == 1);

        REQUIRE(m(0, 0) == 300);
        REQUIRE(m(1, 1) == 20);
        REQUIRE(m(2, 2) == 1);

        REQUIRE(m(0, 1) == 200);
        REQUIRE(m(1, 0) == 30);
        REQUIRE(m(1, 2) == 10);
        REQUIRE(m(2, 1) == 2);
    }
}

TEST_CASE("diagonal of band matrix", "[mafox::BandMatrix]")
{
    BandMatrix<double> m(3, 1, 2);

    m.set_element(0, 0, 1);
    m.set_element(0, 1, 10);
    m.set_element(0, 2, 100);
    m.set_element(1, 0, 2);
    m.set_element(1, 1, 20);
    m.set_element(1, 2, 200);
    m.set_element(2, 1, 30);
    m.set_element(2, 2, 300);

    REQUIRE(m.diagonal_data()[0] == 1);
    REQUIRE(m.diagonal_data()[1] == 20);
    REQUIRE(m.diagonal_data()[2] == 300);

    REQUIRE(m.lower_diagonal_data(0)[0] == 2);
    REQUIRE(m.lower_diagonal_data(0)[1] == 30);

    REQUIRE(m.upper_diagonal_data(0)[0] == 10);
    REQUIRE(m.upper_diagonal_data(0)[1] == 200);
    REQUIRE(m.upper_diagonal_data(1)[0] == 100);
}

TEST_CASE("matrix_t of band matrix", "[mafox::BandMatrix]")
{
    using Matrix = BandMatrix<double>;
    using Expected = BandMatrix<int>;
    static_assert(std::is_same_v<typename Matrix::template matrix_t<int>, Expected>);
}