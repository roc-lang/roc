Thing := [].{
    greeting : Str
    greeting = "hello from sibling package"

    # Deliberate type error: a checker that reaches this sibling package will
    # report a TYPE MISMATCH here. If the package were silently skipped, no
    # error would be surfaced.
    mismatched : U64
    mismatched = "not a number"
}
