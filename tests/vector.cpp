#include "tests.h"

TEST_CASE("Vector creation/assign", "[mafox::Vector]")
{
    Vector<int> v(3);

    REQUIRE(v.dimension() == 3);
    REQUIRE(v(0) == 0);
    REQUIRE(v(0) == 0);
    REQUIRE(v(0) == 0);

    v.set_element(0, 42);
    v.set_element(1, -1);
    v.set_element(2, 112);

    REQUIRE(v(0) == 42);
    REQUIRE(v(1) == -1);
    REQUIRE(v(2) == 112);

    auto shared_data = v.shared_data();

    Vector<int> c = v;
    REQUIRE(c.dimension() == v.dimension());
    REQUIRE(c(0) == 42);
    REQUIRE(c(1) == -1);
    REQUIRE(c(2) == 112);
    REQUIRE(c.shared_data() != shared_data);

    Vector<int> mv = std::move(v);
    REQUIRE(mv.dimension() == 3);
    REQUIRE(mv(0) == 42);
    REQUIRE(mv(1) == -1);
    REQUIRE(mv(2) == 112);
    REQUIRE(mv.shared_data() == shared_data);

    Vector vlist = { 10, 20, 30 };
    REQUIRE(vlist.dimension() == 3);
    REQUIRE(vlist(0) == 10);
    REQUIRE(vlist(1) == 20);
    REQUIRE(vlist(2) == 30);

    Vector<int> vinitial(3, 41);
    REQUIRE(vinitial.dimension() == 3);
    REQUIRE(vinitial(0) == 41);
    REQUIRE(vinitial(1) == 41);
    REQUIRE(vinitial(2) == 41);
}