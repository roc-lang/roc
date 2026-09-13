# Repro for https://github.com/roc-lang/roc/issues/11274: a headerless hello
# world whose entrypoint is named `main` instead of `main!`. A headerless file
# never needs an app header, so `roc` must name the missing `main!` the way
# `roc check` does for this same file, rather than asking for an app header.
main = |_args| {
    echo!("Hello, World!")
    Ok({})
}
