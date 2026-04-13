## Transform module for data transformations.
## Imports Util for string helpers.
## Independent from Validator/Field — provides a separate branch of the import tree.

import Util

Transform := [
    Uppercase,
    Wrap(Str, Str),
    Prefix(Str),
].{

    ## Create a transform that converts to uppercase.
    uppercase : {} -> Transform
    uppercase = |{}| Uppercase

    ## Create a transform that wraps the string with a prefix and suffix.
    wrap : Str, Str -> Transform
    wrap = |prefix_str, suffix_str| Wrap(prefix_str, suffix_str)

    ## Create a transform that adds a prefix.
    prefix : Str -> Transform
    prefix = |p| Prefix(p)

    ## Apply a transform to a value.
    apply : Transform, Str -> Str
    apply = |transform, val|
        match transform {
            Uppercase => {
                bytes = val.to_utf8().map(|b| if b >= 'a' and b <= 'z' b - 32 else b)
                match Str.from_utf8(bytes) {
                    Ok(upper) => upper
                    Err(_) => val
                }
            }
            Wrap(pre, suf) => Util.surround(val, pre, suf)
            Prefix(p) => "${p}${val}"
        }

    ## Apply a list of transforms left to right.
    apply_all : List(Transform), Str -> Str
    apply_all = |transforms, val|
        transforms.fold(val, |acc, t| apply(t, acc))
}

# Tests

expect Transform.apply(Transform.uppercase({}), "hello") == "HELLO"
expect Transform.apply(Transform.uppercase({}), "Hello World") == "HELLO WORLD"

expect Transform.apply(Transform.wrap("[", "]"), "item") == "[item]"
expect Transform.apply(Transform.wrap("<b>", "</b>"), "bold") == "<b>bold</b>"

expect Transform.apply(Transform.prefix(">> "), "line") == ">> line"

expect {
    transforms = [Transform.prefix("hello "), Transform.uppercase({}), Transform.wrap("<", ">")]
    Transform.apply_all(transforms, "world") == "<HELLO WORLD>"
}

expect Transform.apply_all([], "unchanged") == "unchanged"
