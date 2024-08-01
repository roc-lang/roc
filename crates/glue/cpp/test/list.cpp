#include "roc_std.h"
#include "lib/simpletest.h"

namespace Roc {

class SomeValue final : public Roc::Value
{
    int64_t a;
    int64_t b;

public:
    SomeValue(int64_t a, int64_t b) : a(a), b(b) {}

    bool operator==(const SomeValue &other) const
    {
        return a == other.a && b == other.b;
    }
};

DEFINE_TEST_G(FromArrayI64, List)
{
    I64 elems[5] = {I64(1), I64(2), I64(3), I64(4), I64(5)};
    size_t len = sizeof(elems) / sizeof(elems[0]);
    Roc::List<I64> list(elems, len);
    TEST(list.length() == len);
    TEST(list.capacity() == len);
}

DEFINE_TEST_G(FromArrayOfStruct, List)
{
    SomeValue elems[5] = {SomeValue(1, 1),
                          SomeValue(2, 2),
                          SomeValue(3, 3),
                          SomeValue(4, 4),
                          SomeValue(5, 5)};
    size_t len = sizeof(elems) / sizeof(elems[0]);
    Roc::List<SomeValue> list(elems, len);
    TEST(list.length() == len);
    TEST(list.capacity() == len);
}

DEFINE_TEST_G(ReserveSmall, List)
{
    Roc::List<SomeValue> list;
    size_t cap = 42;
    list.reserve(cap);
    TEST_EQ(list.length(), 0);
    TEST_EQ(list.capacity(), cap);
}

DEFINE_TEST_G(Void, List)
{
    Roc::List<void> list(NULL, 12, 42);
    TEST(list.elements() == NULL);
    TEST_EQ(list.length(), 12);
    TEST_EQ(list.capacity(), 42);
}

};
