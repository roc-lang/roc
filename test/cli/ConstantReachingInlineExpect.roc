# `roc test` evaluates `width` at compile time. That evaluation reaches the
# passing inline expect in `checked_width`, which must run as an ordinary
# expect rather than as a test observation, so every test passes.
ConstantReachingInlineExpect := [].{
    checked_width : U64 -> U64
    checked_width = |w| {
        expect w < 100
        w
    }

    width : U64
    width = checked_width(3)
}

expect ConstantReachingInlineExpect.width == 3
expect ConstantReachingInlineExpect.checked_width(4) == 4
