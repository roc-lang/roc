app "hello-world"
    packages { base: "platform" }
    imports []
    provides [ main ] to base

Effect a : {} -> a

always : a -> Effect a
always = \x -> (\{} -> x)

Task ok err : Effect (Result ok err)


succeed : val -> Task val *
succeed = \val ->
    always (Ok val)

fail : val -> Task * val
fail = \val ->
    always (Err val)


map : Task a err, (a -> b) -> Task b err
map = \effect, transform ->
    \{} -> 
        when effect {} is
            Ok a -> (succeed (transform a)) {}
            Err err -> (fail err) {}


greeting =
    x : Task Str []
    x = succeed "foo"

    when map x (\y -> Str.concat y "foo")  is
        _ -> "foobar"

main = greeting
