# Regression test: annotation-only declarations in a non-platform type module
# must NOT be reported as an EFFECTFUL FUNCTION NAME (they perform no effect, so
# they don't need a trailing `!`). They are simply unimplemented, which should be
# reported as DECLARATION HAS NO VALUE instead.
AnnoOnlyNotEffectful := [].{
    starts_with_scalar : Str, U32 -> Bool

    to_scalars : Str -> List(U32)
}

get_scalar_unsafe : Str, U64 -> { scalar : U32, bytes_parsed : U64 }
