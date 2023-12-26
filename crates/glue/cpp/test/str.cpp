#include "roc_std.h"
#include "lib/simpletest.h"
#include <cstring>

bool same_representation(Roc::Str *s, const char *bytes)
{
    char *str_bytes = (char *)s;
    bool ok = 0 == strncmp(str_bytes, bytes, sizeof(Roc::Str));
#ifdef DEBUG
    if (!ok)
    {
        printf("Expected (%p): ", bytes);
        for (size_t i = 0; i < sizeof(Roc::Str); i++)
        {
            printf("%02x ", (unsigned char)bytes[i]);
        }
        printf("\n");
        printf("Actual   (%p): ", str_bytes);
        for (size_t i = 0; i < sizeof(Roc::Str); i++)
        {
            printf("%02x ", (unsigned char)str_bytes[i]);
        }
        printf("\n");
    }
#endif
    return ok;
}

DEFINE_TEST_G(EmptyBytes, Str)
{
    Roc::Str s("");
    char bytes[sizeof(Roc::Str)] = {};
    bytes[sizeof(Roc::Str) - 1] = 0x80;
    TEST(same_representation(&s, bytes));
}

DEFINE_TEST_G(SingleCharBytes, Str)
{
    Roc::Str s("a");
    char bytes[sizeof(Roc::Str)] = {'a'};
    bytes[sizeof(Roc::Str) - 1] = 0x80;
    TEST(same_representation(&s, bytes));
}

DEFINE_TEST_G(MaxSmallStrBytes, Str)
{
    char cstr[sizeof(Roc::Str)] = {};
    memset(cstr, 'a', Roc::SMALL_STRING_MAXLEN);
    Roc::Str s(cstr);

    char bytes[sizeof(Roc::Str)];
    memcpy(bytes, cstr, Roc::SMALL_STRING_MAXLEN);
    bytes[Roc::SMALL_STRING_MAXLEN] = 0x80 | Roc::SMALL_STRING_MAXLEN;

    TEST(same_representation(&s, bytes));
}

DEFINE_TEST_G(ShortLength, Str)
{
    Roc::Str s("hello");
    TEST_EQ(s.length(), 5);
}

DEFINE_TEST_G(LongLength, Str)
{
    Roc::Str s("Hello there, I am a long string.");
    TEST_EQ(s.length(), 32);
}

DEFINE_TEST_G(EmptyLength, Str)
{
    Roc::Str s("");
    TEST_EQ(s.length(), 0);
}

DEFINE_TEST_G(EmptyCapacity, Str)
{
    Roc::Str s("");
    TEST_EQ(s.capacity(), Roc::SMALL_STRING_MAXLEN);
}
