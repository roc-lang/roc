# A package module that also defines `main!`. No malice: the author left an
# entrypoint behind. Inside a package it is an ordinary top-level definition,
# not a published entrypoint root, and must not panic the consumer's compiler.
Helper := [].{
    greet : Str -> Str
    greet = |name| "hi ${name}"
}

main! = |_args| {}
