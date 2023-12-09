#include "roc_std.h"
#include "lib/simpletest.h"

namespace Roc
{

    DEFINE_TEST_G(I16Str, Dict)
    {
        Dict<I16, Str> dict(5);
        I16 ints[5] = {I16(1), I16(2), I16(3), I16(4), I16(5)};
        Str strs[5] = {Str("1"), Str("2"), Str("3"), Str("4"), Str("5")};
        for (size_t i = 0; i < 5; i++)
        {
            dict.insert(ints[i], strs[i]);
        }
        TEST_EQ(dict.length(), 5);
        TEST_EQ(dict.capacity(), 5);

        for (size_t i = 0; i < 5; i++)
        {
            Str *s = dict.get(&ints[i]);
            TEST_EQ(*s, strs[i]);
        }

        TEST_EQ(dict.length(), 5);
        TEST_EQ(dict.capacity(), 5);
        TEST(!(DICT_KEY_FIRST<I16, Str>));
    }

    DEFINE_TEST_G(StrI16, Dict)
    {
        Dict<Str, I16> dict(5);
        I16 ints[5] = {I16(1), I16(2), I16(3), I16(4), I16(5)};
        Str strs[5] = {Str("1"), Str("2"), Str("3"), Str("4"), Str("5")};
        for (size_t i = 0; i < 5; i++)
        {
            dict.insert(strs[i], ints[i]);
        }
        for (size_t i = 0; i < 5; i++)
        {
            I16 *x = dict.get(&strs[i]);
            TEST_EQ(*x, ints[i]);
        }
        TEST_EQ(dict.length(), 5);
        TEST_EQ(dict.capacity(), 5);
        TEST((DICT_KEY_FIRST<Str, I16>));
    }

    DEFINE_TEST_G(I16I16, Dict)
    {
        Dict<I16, I16> dict(5);
        I16 ints[5] = {I16(1), I16(2), I16(3), I16(4), I16(5)};
        for (size_t i = 0; i < 5; i++)
        {
            dict.insert(ints[i], ints[i]);
        }
        for (size_t i = 0; i < 5; i++)
        {
            I16 *x = dict.get(&ints[i]);
            TEST_EQ(*x, ints[i]);
        }
        TEST_EQ(dict.length(), 5);
        TEST_EQ(dict.capacity(), 5);
    }

};
