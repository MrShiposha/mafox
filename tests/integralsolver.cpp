#include "tests.h"

#include <iostream>

TEST_CASE("integrate x^3 - x^2*sin(x)", "[mafox::IntegralSolver]")
{
    auto r = IntegralSolver::gaussian<double>
    (
        [](double x) { return x*x*x - x*x*sin(x); }, 
        0, 10,
        7
    );

    REQUIRE(r == Approx(2430.65));
}