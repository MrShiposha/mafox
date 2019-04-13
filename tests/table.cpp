#include "tests.h"

#define RESET_DEFAULT \
    default_1_constructs = 0; \
    default_1_destructs = 0;  \
    default_2_constructs = 0; \
    default_2_destructs = 0;  \

TEST_CASE("Table creation", "[metaxxa::Table]")
{
    static int default_1_constructs = 0;
    static int default_1_destructs = 0;

    static int default_2_constructs = 0; 
    static int default_2_destructs = 0; 

    struct TestDefault_1
    {
        TestDefault_1()
        {
            ++default_1_constructs;
        }

        TestDefault_1(const TestDefault_1 &other)
        : v(other.v)
        {
            ++default_1_constructs;
        }

        TestDefault_1(int v)
        : v(v)
        {
            ++default_1_constructs;
        }
        
        ~TestDefault_1()
        {
            ++default_1_destructs;
        }

        int v = 42;
    };

    struct TestDefault_2
    {
        TestDefault_2()
        {
            ++default_2_constructs;
        }

        TestDefault_2(const TestDefault_2 &other)
        : v(other.v)
        {
            ++default_2_constructs;
        }

        TestDefault_2(char v)
        : v(v)
        {
            ++default_2_constructs;
        }
        
        ~TestDefault_2()
        {
            ++default_2_destructs;
        }

        char v = 'c';
    };

    SECTION("Default")
    {
        Table<TestDefault_1, TestDefault_2> table;

        static_assert(table.cols() == 2);
        REQUIRE(table.rows() == 0);

        REQUIRE((default_1_constructs == 0 && default_1_destructs == 0));
        REQUIRE((default_2_constructs == 0 && default_2_destructs == 0));
    }

    SECTION("Default 4 elements")
    {
        {
            Table<TestDefault_1, TestDefault_2> table(4);

            static_assert(table.cols() == 2);
            REQUIRE(table.rows() == 4);
            REQUIRE(table.at<0>(0).v == 42);
            REQUIRE(table.at<0>(1).v == 42);
            REQUIRE(table.at<0>(2).v == 42);
            REQUIRE(table.at<0>(3).v == 42);

            REQUIRE(table.at<1>(0).v == 'c');
            REQUIRE(table.at<1>(1).v == 'c');
            REQUIRE(table.at<1>(2).v == 'c');
            REQUIRE(table.at<1>(3).v == 'c');

            REQUIRE(default_1_constructs == 4);
            REQUIRE(default_2_constructs == 4);
        }

        REQUIRE(default_1_destructs == 4);
        REQUIRE(default_2_destructs == 4);
    }

    SECTION("Initial values")
    {
        RESET_DEFAULT
        {
            Table<TestDefault_1, TestDefault_2> table(4, 112, 'a');

            static_assert(table.cols() == 2);
            REQUIRE(table.rows() == 4);
            REQUIRE(table.at<0>(0).v == 112);
            REQUIRE(table.at<0>(1).v == 112);
            REQUIRE(table.at<0>(2).v == 112);
            REQUIRE(table.at<0>(3).v == 112);

            REQUIRE(table.at<1>(0).v == 'a');
            REQUIRE(table.at<1>(1).v == 'a');
            REQUIRE(table.at<1>(2).v == 'a');
            REQUIRE(table.at<1>(3).v == 'a');

            REQUIRE(default_1_constructs - 1 == 4);
            REQUIRE(default_2_constructs - 1 == 4);
        }

        REQUIRE(default_1_destructs - 1 == 4);
        REQUIRE(default_2_destructs - 1 == 4);
    }

    SECTION("Copy")
    {
        RESET_DEFAULT
        {
            Table<TestDefault_1, TestDefault_2> table_old(4, 112, 'a');
            Table<TestDefault_1, TestDefault_2> table(table_old);

            static_assert(table.cols() == 2);
            REQUIRE(table.rows() == 4);
            REQUIRE(table.at<0>(0).v == 112);
            REQUIRE(table.at<0>(1).v == 112);
            REQUIRE(table.at<0>(2).v == 112);
            REQUIRE(table.at<0>(3).v == 112);

            REQUIRE(table.at<1>(0).v == 'a');
            REQUIRE(table.at<1>(1).v == 'a');
            REQUIRE(table.at<1>(2).v == 'a');
            REQUIRE(table.at<1>(3).v == 'a');

            REQUIRE(default_1_constructs/2 == 4);
            REQUIRE(default_2_constructs/2 == 4);
        }

        REQUIRE(default_1_destructs/2 == 4);
        REQUIRE(default_2_destructs/2 == 4);
    }

    SECTION("Move")
    {
        RESET_DEFAULT
        {
            Table<TestDefault_1, TestDefault_2> table_old(4, 112, 'a');
            Table<TestDefault_1, TestDefault_2> table(std::move(table_old));

            static_assert(table.cols() == 2);
            REQUIRE(table.rows() == 4);
            REQUIRE(table.at<0>(0).v == 112);
            REQUIRE(table.at<0>(1).v == 112);
            REQUIRE(table.at<0>(2).v == 112);
            REQUIRE(table.at<0>(3).v == 112);

            REQUIRE(table.at<1>(0).v == 'a');
            REQUIRE(table.at<1>(1).v == 'a');
            REQUIRE(table.at<1>(2).v == 'a');
            REQUIRE(table.at<1>(3).v == 'a');

            REQUIRE(default_1_constructs - 1 == 4);
            REQUIRE(default_2_constructs - 1 == 4);
        }

        REQUIRE(default_1_destructs - 1 == 4);
        REQUIRE(default_2_destructs - 1 == 4);
    }

    SECTION("Assign")
    {
        RESET_DEFAULT
        {
            Table<TestDefault_1, TestDefault_2> table_old(4, 112, 'a');
            Table<TestDefault_1, TestDefault_2> table;
            table = table_old;

            static_assert(table.cols() == 2);
            REQUIRE(table.rows() == 4);
            REQUIRE(table.at<0>(0).v == 112);
            REQUIRE(table.at<0>(1).v == 112);
            REQUIRE(table.at<0>(2).v == 112);
            REQUIRE(table.at<0>(3).v == 112);

            REQUIRE(table.at<1>(0).v == 'a');
            REQUIRE(table.at<1>(1).v == 'a');
            REQUIRE(table.at<1>(2).v == 'a');
            REQUIRE(table.at<1>(3).v == 'a');

            REQUIRE(default_1_constructs/2 == 4);
            REQUIRE(default_2_constructs/2 == 4);
        }

        REQUIRE(default_1_destructs/2 == 4);
        REQUIRE(default_2_destructs/2 == 4);
    }

    SECTION("Move assign")
    {
        RESET_DEFAULT
        {
            Table<TestDefault_1, TestDefault_2> table_old(4, 112, 'a');
            Table<TestDefault_1, TestDefault_2> table;
            table = std::move(table_old);

            static_assert(table.cols() == 2);
            REQUIRE(table.rows() == 4);
            REQUIRE(table.at<0>(0).v == 112);
            REQUIRE(table.at<0>(1).v == 112);
            REQUIRE(table.at<0>(2).v == 112);
            REQUIRE(table.at<0>(3).v == 112);

            REQUIRE(table.at<1>(0).v == 'a');
            REQUIRE(table.at<1>(1).v == 'a');
            REQUIRE(table.at<1>(2).v == 'a');
            REQUIRE(table.at<1>(3).v == 'a');

            REQUIRE(default_1_constructs - 1 == 4);
            REQUIRE(default_2_constructs -1 == 4);
        }

        REQUIRE(default_1_destructs - 1 == 4);
        REQUIRE(default_2_destructs - 1 == 4);
    }
}