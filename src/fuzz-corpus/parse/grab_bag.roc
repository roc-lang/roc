app [main!] { pf: platform "../basic-cli/platform.roc" }

import pf.Stdout

Map a b : List(a), (a -> b) -> List(b)

Foo : (Bar, Baz)

Some a : { foo : Ok(a), bar : Something }

Maybe a : [Some(a), None]

SomeFunc a : Maybe(a), a -> Maybe(a)

add_one_oneline = |num| if num 2 else 5

add_one : (U64 -> U64)
add_one = |num| {
    other = 1
    if num {
        dbg some_func()
        0
    } else {
        dbg 123
        other
    }
}

match_time = |a| match a {
    Blue | Green | Red -> {
        x = 12
        x
    }
    lower -> 1
    "foo" -> 100
    "foo" | "bar" -> 200
    [1, 2, 3, .. as rest] -> 123
    [1, 2 | 5, 3, .. as rest] -> 123
    3.14 -> 314
    3.14 | 6.28 -> 314
    (1, 2, 3) -> 123
    (1, 2 | 5, 3) -> 123
    { foo: 1, bar: 2, ..rest } -> 12
    { foo: 1, bar: 2 | 7 } -> 12
    Ok(123) -> 123
    Ok(Some(dude)) -> dude
    TwoArgs("hello", Some("world")) -> 1000
}

expect blah == 1

main! : List(String) -> Result({}, _)
main! = |_| {
    world = "World"
    number = 123
    expect blah == 1
    tag = Blue
    return tag
    ...
    match_time(...)
    crash "Unreachable!"
    tag_with_payload = Ok(number)
    interpolated = "Hello, ${world}"
    list = [add_one(number), 456, 789]
    record = { foo: 123, bar: "Hello", baz: tag, qux: Ok(world), punned }
    tuple = (123, "World", tag, Ok(world), (nested, tuple), [1, 2, 3])
    bin_op_result = Err(foo) ?? 12 > 5 * 5 or 13 + 2 < 5 and 10 - 1 >= 16 or 12 <= 3 / 5
    static_dispatch_style = some_fn(arg1)?.static_dispatch_method()?.next_static_dispatch_method()?.record_field?
    Stdout.line!(interpolated)?
    Stdout.line!("How about ${Num.toStr(number)} as a string?")
}

expect {
    foo = 1
    blah = 1
    blah == foo
}

