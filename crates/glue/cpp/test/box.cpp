#include "roc_std.h"
#include "lib/simpletest.h"

DEFINE_TEST_G(Deref, Box)
{
    const Roc::I64 i64(12345);
    Roc::Box<Roc::I64> box(i64);
    const Roc::I64 box_deref = *box;
    TEST_EQ(box_deref.value, i64.value);
}

DEFINE_TEST_G(Arrow, Box)
{
    const Roc::I64 i64(12345);
    Roc::Box<Roc::I64> box(i64);
    TEST_EQ(box->value, i64.value);
}
