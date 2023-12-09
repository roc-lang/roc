#include "roc_std.h"
#include "lib/simpletest.h"

namespace Roc
{

    DEFINE_TEST_G(Ok, Result)
    {
        const Str str("It's all good, no errors in this long string!");
        const Result<Str, I64> ok = Result<Str, I64>::make_ok(str);
        TEST(ok.is_ok());
    }

    DEFINE_TEST_G(Err, Result)
    {
        const Str str("Oops, this long string is about a nasty error!");
        const Result<I64, Str> err = Result<I64, Str>::make_err(str);
        TEST(!err.is_ok());
    }

    DEFINE_TEST_G(OkSameType, Result)
    {
        const Str str("It's all good, no errors in this long string!");
        const Result<Str, Str> ok = Result<Str, Str>::make_ok(str);
        TEST(ok.is_ok());
    }

    DEFINE_TEST_G(ErrSameType, Result)
    {
        const Str str("Oops, this long string is about a nasty error!");
        const Result<Str, Str> err = Result<Str, Str>::make_err(str);
        TEST(!err.is_ok());
    }
};
