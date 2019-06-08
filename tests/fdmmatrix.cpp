#include "tests.h"

TEST_CASE("fdm_matrix")
{
    auto m = fdm_matrix(1, 4, 1)
        .a_coeff([](auto x) { return -x; })
        .c_coeff([](auto x) { return x*10; })
        .b_coeff([](auto x) { return x; })
        .compute();

    std::cout << std::setw(5) << m << std::endl;
}