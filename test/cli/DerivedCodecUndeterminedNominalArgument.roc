DerivedCodecUndeterminedNominalArgument :: [].{}

# A user nominal's derived codec reads its `err` argument as a tag payload,
# and nothing in the program ever determines that argument, so neither codec
# can be derived.

MyTry(ok, err) := [Ok(ok), Err(err)].{
    encoder_for : _
    parser_for : _
}

parsed : Try({ x : U64, y : MyTry(U64, _) }, _)
parsed = Json.parse("{\"x\":1}")

value : { x : U64, y : MyTry(U64, _) }
value = { x: 1, y: MyTry.Ok(2) }

expect parsed.is_ok()
expect Json.to_str(value) == "{\"x\":1,\"y\":{\"Ok\":2}}"
