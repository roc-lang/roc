Issue10121Fn := {}

Shape : {
    item : Try({ bar : Str, count : U64 }, [Missing]),
}

run : Str -> Bool
run = |bar| {
    ok_original : Shape
    ok_original = {
        item: Ok({ bar, count: 1 }),
    }
    missing_original : Shape
    missing_original = {
        item: Err(Missing),
    }

    ok_encoded = Json.to_str(ok_original)
    missing_encoded = Json.to_str(missing_original)
    ok_parsed : Try(Shape, [InvalidJson(Str), MissingRequiredField(Str)])
    ok_parsed = Json.parse(ok_encoded)
    missing_parsed : Try(Shape, [InvalidJson(Str), MissingRequiredField(Str)])
    missing_parsed = Json.parse(missing_encoded)

    ok_round_trips =
        match ok_parsed {
            Ok(value) => Json.to_str(value) == ok_encoded
            Err(_) => False
        }
    missing_round_trips =
        match missing_parsed {
            Ok(value) => Json.to_str(value) == missing_encoded
            Err(_) => False
        }
    ok_round_trips and missing_round_trips
}

expect {
    run("one")
}
