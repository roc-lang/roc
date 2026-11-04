# A literal reaches a function through a top-level name that is just another
# procedure, both as an argument to the alias and as an argument forwarded
# through a method defined as a top-level function.
pair = |value| ("capture", value)
generic = pair

expect generic(42) == ("capture", 42)

helper = |_self, n| n + 1

Foo := [F].{
    go = helper
}

outer = |m| Foo.F.go(m)

expect outer(2.U64) == 3
