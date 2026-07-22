JsonU8RoundTrip :: [].{}

round_trip : U8 -> Bool
round_trip = |value| {
    encoded = Json.to_str(value)

    parsed : Try(U8, Json.ParseErr)
    parsed = Json.parse(encoded)

    parsed == Ok(value)
}

expect {
    value : U8
    value = 0
    Json.to_str(value) == "0"
}

expect {
    parsed : Try(U8, Json.ParseErr)
    parsed = Json.parse("0")
    parsed == Ok(0)
}

expect {
    value = U8.highest
    encoded = Json.to_str(value)

    parsed : Try(U8, Json.ParseErr)
    parsed = Json.parse(encoded)

    parsed == Ok(value)
}

expect round_trip(U8.lowest) and round_trip(U8.highest)
