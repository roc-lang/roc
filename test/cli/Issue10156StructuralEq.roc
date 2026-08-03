Issue10156StructuralEq :: [].{}

# Repro for https://github.com/roc-lang/roc/issues/10156
expect Str.from_utf8([]) == Ok("")
