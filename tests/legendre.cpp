#include "tests.h"

#include <iostream>
#include <iomanip>

TEST_CASE("P_n(x)", "[mafox::LegendrePolynomial]")
{
    LegendrePolynomial<float> polynomial;

    REQUIRE(polynomial(0.55f) == Approx(1.f));
    REQUIRE(polynomial.power(1)(0.55f) == Approx(0.55f));
    REQUIRE(polynomial.power(2)(0.55f) == Approx(-0.04625f));
    REQUIRE(polynomial.power(3)(0.55f) == Approx(-0.40906f));

    REQUIRE(polynomial.power(10)(0.2f) == Approx(0.12907f).epsilon(0.001));
}

TEST_CASE("D[P_n(x)]", "[mafox::LegendrePolynomial]")
{
    LegendrePolynomial<float> polynomial;

    REQUIRE(polynomial.derivative(0.55f) == Approx(0.f));
    REQUIRE(polynomial.power(1).derivative(0.55f) == Approx(1.f));
    REQUIRE(polynomial.power(2).derivative(0.55f) == Approx(1.65f));
    REQUIRE(polynomial.power(3).derivative(0.55f) == Approx(0.76875));

    REQUIRE(polynomial.power(10).derivative(0.2f) == Approx(2.29315f));
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
        Approx r1(-0.5773502692);
        Approx r2(0.5773502692);

        r1.epsilon(EPS);
        r2.epsilon(EPS);

        polynomial.power(2);
        auto r = polynomial.roots(EPS);
        REQUIRE(r.size() == 2);
        REQUIRE(r[0] == r1);
        REQUIRE(r[1] == r2);
    }

    {
        Approx rleft(-0.7745966692);
        Approx rright(0.7745966692);

        rright.epsilon(EPS);
        rleft.epsilon(EPS);

        polynomial.power(3);
        auto r = polynomial.roots(EPS);
        REQUIRE(r.size() == 3);
        REQUIRE(r[0] == rleft);
        REQUIRE(r[1] == Approx(0.0));
        REQUIRE(r[2] == rright);
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
        REQUIRE(r[0] == r1);
        REQUIRE(r[1] == r2);
        REQUIRE(r[2] == r3);
        REQUIRE(r[3] == r4);
    }
}