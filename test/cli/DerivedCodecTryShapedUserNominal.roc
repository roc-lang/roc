DerivedCodecTryShapedUserNominal :: [].{}

# The `[Null]` and `[Missing]` codec conventions belong to the builtin `Try`.
# A user nominal shaped like `Try` is an ordinary tag union to its derived
# codecs, wherever it appears and whatever its error row holds.

MyTry(ok, err) := [Ok(ok), Err(err)].{
    encoder_for : _
    parser_for : _
}

null_field : { x : U64, y : MyTry(U64, [Null]) }
null_field = { x: 1, y: MyTry.Err(Null) }

null_ok_field : { x : U64, y : MyTry(U64, [Null]) }
null_ok_field = { x: 1, y: MyTry.Ok(2) }

null_top : MyTry(U64, [Null])
null_top = MyTry.Err(Null)

null_list : List(MyTry(U64, [Null]))
null_list = [MyTry.Ok(2), MyTry.Err(Null)]

missing_field : { x : U64, y : MyTry(U64, [Missing]) }
missing_field = { x: 1, y: MyTry.Err(Missing) }

both_list : List(MyTry(U64, [Missing, Null]))
both_list = [MyTry.Ok(3), MyTry.Err(Null), MyTry.Err(Missing)]

expect Json.to_str(null_field) == "{\"x\":1,\"y\":{\"Err\":\"Null\"}}"
expect Json.to_str(null_ok_field) == "{\"x\":1,\"y\":{\"Ok\":2}}"
expect Json.to_str(null_top) == "{\"Err\":\"Null\"}"
expect Json.to_str(null_list) == "[{\"Ok\":2},{\"Err\":\"Null\"}]"
expect Json.to_str(missing_field) == "{\"x\":1,\"y\":{\"Err\":\"Missing\"}}"
expect Json.to_str(both_list) == "[{\"Ok\":3},{\"Err\":\"Null\"},{\"Err\":\"Missing\"}]"

expect {
    parsed : Try({ x : U64, y : MyTry(U64, [Null]) }, _)
    parsed = Json.parse("{\"x\":1,\"y\":{\"Err\":\"Null\"}}")
    match parsed {
        Ok(value) => Json.to_str(value) == "{\"x\":1,\"y\":{\"Err\":\"Null\"}}"
        Err(_) => False
    }
}

# JSON `null` is not one of the user nominal's tags.
expect {
    parsed : Try({ x : U64, y : MyTry(U64, [Null]) }, _)
    parsed = Json.parse("{\"x\":1,\"y\":null}")
    parsed.is_err()
}

expect {
    parsed : Try(MyTry(U64, [Null]), _)
    parsed = Json.parse("null")
    parsed.is_err()
}

# A bare payload is not one of its tags either.
expect {
    parsed : Try(MyTry(U64, [Null]), _)
    parsed = Json.parse("4")
    parsed.is_err()
}

expect {
    parsed : Try(List(MyTry(U64, [Missing, Null])), _)
    parsed = Json.parse("[{\"Ok\":5},{\"Err\":\"Missing\"},{\"Err\":\"Null\"}]")
    match parsed {
        Ok(items) => Json.to_str(items) == "[{\"Ok\":5},{\"Err\":\"Missing\"},{\"Err\":\"Null\"}]"
        Err(_) => False
    }
}

# A required field of the user nominal, absent from the input.
expect {
    parsed : Try({ x : U64, y : MyTry(U64, [Missing, Null]) }, _)
    parsed = Json.parse("{\"x\":1}")
    parsed.is_err()
}
