# `page` is evaluated at compile time by running `B.script` from B's module
# object, where `Html.from_interpolation` rejects the literal. The failure is
# classified from the meaning B's object recorded, whether B's object was
# built in this compilation or reused from one that saw a different A.
import B
import Html

page : Str
page = Html.to_str(B.script(1))

main! = |_args| {
    echo!(page)
    Ok({})
}
