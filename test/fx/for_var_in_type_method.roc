# Bug #9118: segfault when ? operator is used on tuple in type method
# Minimal reproduction

Value := [VAtom(Str), ..].{
    parse : Str -> Try(Value, Str)
    parse = |s| {
        (token, _) = make_value(s)?
        Ok(token)
    }
}

make_value : Str -> (Value, [Ok, Err(Str)])
make_value = |s| (VAtom(s), Ok)

expect {
    result = Value.parse("hello")
    match result {
        Ok(VAtom("hello")) => Bool.True
        _ => Bool.False
    }
}
