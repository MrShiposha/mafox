#include "tests.h"

TEST_CASE("P_n(x)", "[mafox::LegendrePolynomial]")
{
    LegendrePolynomial<float> polynomial;

    REQUIRE(polynomial(15.f) == Approx(1.f));
    REQUIRE(polynomial.power(1)(15.f) == Approx(15.f));
    REQUIRE(polynomial.power(2)(15.f) == Approx(337.f));
    REQUIRE(polynomial.power(3)(15.f) == Approx(8415.f));

    REQUIRE(polynomial.power(10)(2.f) == Approx(96060.51953125f));

}

TEST_CASE("D[P_n(x)]", "[mafox::LegendrePolynomial]")
{
    LegendrePolynomial<float> polynomial;

    REQUIRE(polynomial.derivative(15.f) == Approx(0.f));
    REQUIRE(polynomial.power(1).derivative(15.f) == Approx(1.f));
    REQUIRE(polynomial.power(2).derivative(15.f) == Approx(45.f));
    REQUIRE(polynomial.power(3).derivative(15.f) == Approx(1686.f));

    REQUIRE(polynomial.power(10).derivative(2.f) == Approx(550067.890625f));

}