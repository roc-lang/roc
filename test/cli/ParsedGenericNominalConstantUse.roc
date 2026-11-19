ParsedGenericNominalConstantUse :: [].{}

# A top-level constant parsed into a generic nominal backed by a tag union,
# whose type only a later top-level constant fixes. Solving the specialized
# compile-time program lifts the parser's structural value into the nominal,
# and the nominal's backing must stay that structural tag union.

Opt(a) := [Some(a), None].{
    encoder_for : _
    parser_for : _
}

MyTry(ok, err) := [Ok(ok), Err(err)].{
    encoder_for : _
    parser_for : _
}

some = Json.parse("{\"Some\":1}")

some_opt : Try(Opt(U64), _)
some_opt = some

none = Json.parse("\"None\"")

none_opt : Try(Opt(Str), _)
none_opt = none

err = Json.parse("{\"Err\":\"Bad\"}")

err_try : Try(MyTry(U64, [Bad]), _)
err_try = err

expect match some_opt {
    Ok(value) => Json.to_str(value) == "{\"Some\":1}"
    Err(_) => False
}

expect match none_opt {
    Ok(value) => Json.to_str(value) == "\"None\""
    Err(_) => False
}

expect match err_try {
    Ok(value) => Json.to_str(value) == "{\"Err\":\"Bad\"}"
    Err(_) => False
}
