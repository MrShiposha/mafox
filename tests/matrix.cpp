// #include "tests.h"

// TEST_CASE("creation", "[mafox::Matrix]")
// {
//     SECTION("simple")
//     {
//         Matrix<double> m(3, 2);

//         REQUIRE(m.rows() == 3);
//         REQUIRE(m.cols() == 2);

//         REQUIRE(m.element(0, 0) == Approx(0.0));
//         REQUIRE(m.element(0, 1) == Approx(0.0));

//         REQUIRE(m.element(1, 0) == Approx(0.0));
//         REQUIRE(m.element(1, 1) == Approx(0.0));

//         REQUIRE(m.element(2, 0) == Approx(0.0));
//         REQUIRE(m.element(2, 1) == Approx(0.0));
//     }

//     SECTION("copy")
//     {
//         Matrix<double> m(3, 2);
//         m.set_element(0, 0, 0.01);

//         Matrix<double> n = m;

//         REQUIRE(n.rows() == 3);
//         REQUIRE(n.cols() == 2);

//         REQUIRE(n.element(0, 0) == Approx(0.01));
//         REQUIRE(n.element(0, 1) == Approx(0.0));

//         REQUIRE(n.element(1, 0) == Approx(0.0));
//         REQUIRE(n.element(1, 1) == Approx(0.0));

//         REQUIRE(n.element(2, 0) == Approx(0.0));
//         REQUIRE(n.element(2, 1) == Approx(0.0));

//         REQUIRE(n.data() != m.data());
//     }

//     SECTION("move")
//     {
//         Matrix<double> m(3, 2);
//         m.set_element(0, 0, 0.01);

//         auto *data = m.data();

//         Matrix<double> n = std::move(m);

//         REQUIRE(n.rows() == 3);
//         REQUIRE(n.cols() == 2);

//         REQUIRE(n.element(0, 0) == Approx(0.01));
//         REQUIRE(n.element(0, 1) == Approx(0.0));

//         REQUIRE(n.element(1, 0) == Approx(0.0));
//         REQUIRE(n.element(1, 1) == Approx(0.0));

//         REQUIRE(n.element(2, 0) == Approx(0.0));
//         REQUIRE(n.element(2, 1) == Approx(0.0));
        
//         REQUIRE(n.data() == data);
//     }
// }

// TEST_CASE("assign", "[mafox::Matrix/mafox::AMatrix]")
// {
//     Matrix<double> m(3, 3);

//     m.set_element(0, 0, 3.14);
//     REQUIRE(m.element(0, 0) == Approx(3.14));
//     REQUIRE(m.try_set_element(0, 0, 3.14));

//     SECTION("copy")
//     {
//         Matrix<double> n(1, 1);
        
//         n = m;
//         REQUIRE(n.rows() == 3);
//         REQUIRE(n.cols() == 3);
//         REQUIRE(n.element(0, 0) == Approx(3.14));
//         REQUIRE(n.data() != m.data());
//     }

//     SECTION("move")
//     {
//         Matrix<double> n(1, 1);
//         auto *data = m.data();
        
//         n = std::move(m);
//         REQUIRE(n.rows() == 3);
//         REQUIRE(n.cols() == 3);
//         REQUIRE(n.element(0, 0) == Approx(3.14));
//         REQUIRE(n.data() == data);
//     }
// }

// TEST_CASE("transpose", "[mafox::Matrix]")
// {
//     SECTION("non-square transpose")
//     {
//         Matrix<double> m(3, 2);
//         m.set_element(0, 0, 1);
//         m.set_element(0, 1, 10);

//         m.set_element(1, 0, 2);
//         m.set_element(1, 1, 20);

//         m.set_element(2, 0, 3);
//         m.set_element(2, 1, 30);

//         m.transpose();

//         REQUIRE(m.rows() == 2);
//         REQUIRE(m.cols() == 3);

//         REQUIRE(m.element(0, 0) == Approx(1));
//         REQUIRE(m.element(0, 1) == Approx(2));
//         REQUIRE(m.element(0, 2) == Approx(3));

//         REQUIRE(m.element(1, 0) == Approx(10));
//         REQUIRE(m.element(1, 1) == Approx(20));
//         REQUIRE(m.element(1, 2) == Approx(30));
//     }

//     SECTION("square transpose")
//     {
//         Matrix<double> m(3, 3);
//         m.set_element(0, 0, 1);
//         m.set_element(0, 1, 10);
//         m.set_element(0, 2, 100);

//         m.set_element(1, 0, 2);
//         m.set_element(1, 1, 20);
//         m.set_element(1, 2, 200);

//         m.set_element(2, 0, 3);
//         m.set_element(2, 1, 30);
//         m.set_element(2, 2, 300);

//         m.transpose();

//         REQUIRE(m.rows() == 3);
//         REQUIRE(m.cols() == 3);

//         REQUIRE(m.element(0, 0) == Approx(1));
//         REQUIRE(m.element(0, 1) == Approx(2));
//         REQUIRE(m.element(0, 2) == Approx(3));

//         REQUIRE(m.element(1, 0) == Approx(10));
//         REQUIRE(m.element(1, 1) == Approx(20));
//         REQUIRE(m.element(1, 2) == Approx(30));

//         REQUIRE(m.element(2, 0) == Approx(100));
//         REQUIRE(m.element(2, 1) == Approx(200));
//         REQUIRE(m.element(2, 2) == Approx(300));
//     }

//     SECTION("non-square transpose relative to side diagonal")
//     {
//         Matrix<double> m(3, 2);
//         m.set_element(0, 0, 1);
//         m.set_element(0, 1, 10);

//         m.set_element(1, 0, 2);
//         m.set_element(1, 1, 20);

//         m.set_element(2, 0, 3);
//         m.set_element(2, 1, 30);

//         m.transpose_rsd();

//         REQUIRE(m.rows() == 2);
//         REQUIRE(m.cols() == 3);

//         REQUIRE(m.element(0, 0) == Approx(30));
//         REQUIRE(m.element(0, 1) == Approx(20));
//         REQUIRE(m.element(0, 2) == Approx(10));

//         REQUIRE(m.element(1, 0) == Approx(3));
//         REQUIRE(m.element(1, 1) == Approx(2));
//         REQUIRE(m.element(1, 2) == Approx(1));
//     }

//     SECTION("square transpose relative to side diagonal")
//     {
//         Matrix<double> m(3, 3);
//         m.set_element(0, 0, 1);
//         m.set_element(0, 1, 10);
//         m.set_element(0, 2, 100);

//         m.set_element(1, 0, 2);
//         m.set_element(1, 1, 20);
//         m.set_element(1, 2, 200);

//         m.set_element(2, 0, 3);
//         m.set_element(2, 1, 30);
//         m.set_element(2, 2, 300);

//         m.transpose_rsd();

//         REQUIRE(m.rows() == 3);
//         REQUIRE(m.cols() == 3);

//         REQUIRE(m.element(0, 0) == Approx(300));
//         REQUIRE(m.element(0, 1) == Approx(200));
//         REQUIRE(m.element(0, 2) == Approx(100));

//         REQUIRE(m.element(1, 0) == Approx(30));
//         REQUIRE(m.element(1, 1) == Approx(20));
//         REQUIRE(m.element(1, 2) == Approx(10));

//         REQUIRE(m.element(2, 0) == Approx(3));
//         REQUIRE(m.element(2, 1) == Approx(2));
//         REQUIRE(m.element(2, 2) == Approx(1));
//     }
// }

// TEST_CASE("element", "[mafox::Matrix]")
// {
//     Matrix<double> m(2, 3);
//     m.set_element(0, 0, 0.123);
//     m.set_element(1, 0, 0.111);

//     REQUIRE(m.element(0, 0) == m(0, 0));
//     REQUIRE(m.element(0, 1) == m(0, 1));
//     REQUIRE(m.element(1, 0) == m(1, 0));
//     REQUIRE(m.element(1, 1) == m(1, 1));
// }

// TEST_CASE("set_order", "[mafox::Matrix]")
// {
//     Matrix<double> m(2, 3);
//     m.set_element(0, 0, 0.123);
//     m.set_element(1, 0, 0.111);

//     Matrix<double> m1(2, 3, COL_MAJOR);
//     m1.set_element(1, 0, 0.111);

//     REQUIRE(m.data()[1*m.cols() + 0] == m1.data()[0*m1.rows() + 1]);

//     m1.set_order(ROW_MAJOR);
//     REQUIRE(m1(1, 0) == 0.111);
// }

// TEST_CASE("share", "[mafox::Matrix]")
// {
//     Matrix<double> m(2, 3);
//     Matrix<double> m1 = m.share();

//     REQUIRE(m.shared_data() == m1.shared_data());
//     REQUIRE(m.data() == m1.data());
// }