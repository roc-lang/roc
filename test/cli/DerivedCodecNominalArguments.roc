DerivedCodecNominalArguments :: [].{}

# Derived codecs for user nominals whose arguments are all determined, whose
# undetermined argument the backing never reads, or whose undetermined
# argument only an encoder meets.

MyTry(ok, err) := [Ok(ok), Err(err)].{
    encoder_for : _
    parser_for : _
}

Tagged(phantom) := { x : U64 }.{
    encoder_for : _
    parser_for : _
}

Wrap(a) := { x : a }.{
    encoder_for : _
    parser_for : _
}

# A required field of a user nominal type, absent from the input.
missing : Try({ x : U64, y : MyTry(U64, [Missing]) }, _)
missing = Json.parse("{\"x\":1}")

present : Try({ x : U64, y : MyTry(U64, [Missing]) }, _)
present = Json.parse("{\"x\":1,\"y\":{\"Ok\":2}}")

# A user nominal is not the builtin `Try`, so its field is read as its own
# tag union, never as a bare payload.
bare : Try({ x : U64, y : MyTry(U64, [Missing]) }, _)
bare = Json.parse("{\"x\":1,\"y\":2}")

value : { x : U64, y : MyTry(U64, [Missing]) }
value = { x: 1, y: MyTry.Ok(2) }

# Nothing determines the `err` argument, so no value has the `Err` tag and the
# encoder has no branch for it.
undetermined : { x : U64, y : MyTry(U64, _) }
undetermined = { x: 1, y: MyTry.Ok(2) }

tagged : Try(Tagged(_), _)
tagged = Json.parse("{\"x\":3}")

tagged_value : Tagged(_)
tagged_value = Tagged.{ x: 4 }

# The argument is a variable of a generic function, determined at each use.
encode_wrapped = |v| Json.to_str(Wrap.{ x: v })

parse_any = |s| Json.parse(s)

wrapped : Try(Wrap(U64), _)
wrapped = parse_any("{\"x\":5}")

expect missing.is_err()
expect present.is_ok()
expect bare.is_err()
expect Json.to_str(value) == "{\"x\":1,\"y\":{\"Ok\":2}}"
expect Json.to_str(undetermined) == "{\"x\":1,\"y\":{\"Ok\":2}}"
expect tagged.is_ok()
expect Json.to_str(tagged_value) == "{\"x\":4}"
expect encode_wrapped("s") == "{\"x\":\"s\"}"
expect encode_wrapped(6.U64) == "{\"x\":6}"
expect wrapped.is_ok()
