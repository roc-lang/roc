# Both roots depending on the erroneous initializer count as compiler errors.
bad : U64
bad = "bad"

expect bad == 1
expect bad == 2
expect Bool.True
