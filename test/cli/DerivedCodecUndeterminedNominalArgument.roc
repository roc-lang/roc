DerivedCodecUndeterminedNominalArgument :: [].{}

# A user nominal's derived parser reads its `err` argument as a tag payload,
# and nothing in the program ever determines that argument, so the parser
# cannot be derived.

MyTry(ok, err) := [Ok(ok), Err(err)].{
    encoder_for : _
    parser_for : _
}

parsed : Try({ x : U64, y : MyTry(U64, _) }, _)
parsed = Json.parse("{\"x\":1}")

expect parsed.is_ok()
