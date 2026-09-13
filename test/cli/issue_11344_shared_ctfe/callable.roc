import CallableHelper

# A concrete captured function has one evaluated root representation, including
# when passed through containers. A generic function alias remains a scheme.
make_renderer : Str -> (Str -> Str)
make_renderer = |prefix| {
    |suffix| "${prefix}${suffix}"
}

fixed : Str -> Str
fixed = make_renderer("shared:")

nested = { values: [fixed] }

pair = |value| ("capture", value)
generic = pair

expect fixed("value") == "shared:value"
expect List.map(nested.values, |function| function("value")) == ["shared:value"]
expect generic(42) == ("capture", 42)
expect generic("text") == ("capture", "text")

expect CallableHelper.render("app") == "an imported compile-time capture:app"
