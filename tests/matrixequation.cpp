// #include "tests.h"

// TEST_CASE("MatrixEquation(tridiagonal)")
// {
//     TridiagonalMatrix<double> m(3);
//     m.set_element(0, 0, 1);
//     m.set_element(0, 1, -2);

//     m.set_element(1, 0, 3);
//     m.set_element(1, 1, 4);
//     m.set_element(1, 2, 5);

//     m.set_element(2, 1, 6);
//     m.set_element(2, 2, 7);

//     Vector<double> v(3);
//     v.set_element(0, 100);
//     v.set_element(1, 200);
//     v.set_element(2, 300);

//     auto solution = (m*X = v).solve();

//     REQUIRE(solution(0) == Approx(-10));
//     REQUIRE(solution(1) == Approx(-55));
//     REQUIRE(solution(2) == Approx(90));

// }