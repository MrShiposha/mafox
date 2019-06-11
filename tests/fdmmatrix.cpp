// #include "tests.h"

// TEST_CASE("fdm_matrix")
// {
//     auto m = fdm_matrix(1, 4, 1)
//         .a_coeff([](auto x) { return -x; })
//         .c_coeff([](auto x) { return x*10; })
//         .b_coeff([](auto x) { return x; })
//         .compute();

//     REQUIRE(m(0, 0) == 1);
//     REQUIRE(m(1, 1) == 2);
//     REQUIRE(m(2, 2) == 3);

//     REQUIRE(m(0, 1) == 10);
//     REQUIRE(m(1, 2) == 20);

//     REQUIRE(m(1, 0) == -2);
//     REQUIRE(m(2, 1) == -3);
// }