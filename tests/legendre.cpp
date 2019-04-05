#include "tests.h"

#include <iostream>
#include <iomanip>

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

TEST_CASE("roots of P_n(x)", "[mafox::LegendrePolynomial]")
{
    LegendrePolynomial polynomial;
    constexpr double EPS = 0.0000000001;

    {
        auto r = polynomial.roots(EPS);
        REQUIRE(r.empty());
    }

    {
        polynomial.power(1);
        auto r = polynomial.roots(EPS);
        REQUIRE(r.size() == 1);
        REQUIRE(r[0] == Approx(0.0));
    }

    {
        Approx r1(0.5773502692);
        Approx r2(-0.5773502692);

        r1.epsilon(EPS);
        r2.epsilon(EPS);

        polynomial.power(2);
        auto r = polynomial.roots(EPS);
        REQUIRE(r.size() == 2);
        REQUIRE(r[0] == r1);
        REQUIRE(r[1] == r2);
    }

    {
        Approx rright(0.7745966692);
        Approx rleft(-0.7745966692);

        rright.epsilon(EPS);
        rleft.epsilon(EPS);

        polynomial.power(3);
        auto r = polynomial.roots(EPS);
        REQUIRE(r.size() == 3);
        REQUIRE(r[0] == rright);
        REQUIRE(r[1] == Approx(0.0));
        REQUIRE(r[2] == rleft);
    }

    {
        Approx r4(0.8611363116);
        Approx r3(0.3399810436);
        Approx r2(-0.3399810436);
        Approx r1(-0.8611363116);

        r4.epsilon(EPS);
        r3.epsilon(EPS);
        r2.epsilon(EPS);
        r1.epsilon(EPS);

        polynomial.power(4);
        auto r = polynomial.roots(EPS);
        REQUIRE(r.size() == 4);
        REQUIRE(r[0] == r4);
        REQUIRE(r[1] == r3);
        REQUIRE(r[2] == r2);
        REQUIRE(r[3] == r1);
    }
}