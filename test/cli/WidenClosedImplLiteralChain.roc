# A requirement's dictionary handed on inside a literal's conversion. The
# literal `5 : Wrapper(a)` converts where `a` becomes concrete, in the frame
# of the function that wrote it, and `Wrapper.from_numeral` takes that
# function's `label` dictionary at a wider row than the function's own
# requirement states. The frame holds the dictionary at its own requirement,
# so it reaches the conversion adapted to the wider row, or `Ok` is read back
# under the wider row's numbering as `Extra`. Compile-time evaluation converts
# the literal under either strategy, so this runs the same path under both.
WidenClosedImplLiteralChain := {}

Job := [Pending].{
    label : () -> [Ok(Str)]
    label = || Ok("cv")
}

show : [Ok(Str), Err(Str), Extra] -> Str
show = |v| match v { Ok(s) => "Ok(${s})", Err(e) => "Err(${e})", Extra => "Extra" }

Wrapper(a) := [Wrapped(Str)].{
    from_numeral : Numeral -> Try(Wrapper(a), [InvalidNumeral(Str)]) where [a.label : () -> [Ok(Str), Err(Str), Extra]]
    from_numeral = |_| {
        Shape : a
        Ok(Wrapper.Wrapped(show(Shape.label())))
    }

    text : Wrapper(a) -> Str
    text = |Wrapper.Wrapped(s)| s
}

# The literal's frame hands its dictionary to the conversion.
wrapped : a -> Wrapper(a) where [a.label : () -> [Ok(Str), Err(Str)]]
wrapped = |_| 5

# The literal is forwarded through a second frame, which hands its dictionary
# to the first at a wider row.
outer : a -> Wrapper(a) where [a.label : () -> [Ok(Str)]]
outer = |x| wrapped(x)

# The second frame is an alias's scope, entered from a closed use and from a
# generic function's.
wrapped_alias : a -> Wrapper(a) where [a.label : () -> [Ok(Str)]]
wrapped_alias = wrapped

through_alias = |x| wrapped_alias(x)

# The conversion is bound to an alias, and the function it forwards to holds
# a literal of its own: that literal's chain enters the alias's scope and the
# forwarded function's frame from the conversion's dispatch edge.
Nested(a) := [Nested(Str)].{
    convert : Numeral -> Try(Nested(a), [InvalidNumeral(Str)]) where [a.label : () -> [Ok(Str), Err(Str)]]
    convert = |_| {
        inner : Wrapper(a)
        inner = 7
        Ok(Nested.Nested(inner.text()))
    }

    from_numeral : Numeral -> Try(Nested(a), [InvalidNumeral(Str)]) where [a.label : () -> [Ok(Str)]]
    from_numeral = convert

    text : Nested(a) -> Str
    text = |Nested.Nested(s)| s
}

nested : a -> Nested(a) where [a.label : () -> [Ok(Str)]]
nested = |_| 5

expect wrapped(Job.Pending).text() == "Ok(cv)"
expect outer(Job.Pending).text() == "Ok(cv)"
expect wrapped_alias(Job.Pending).text() == "Ok(cv)"
expect through_alias(Job.Pending).text() == "Ok(cv)"
expect nested(Job.Pending).text() == "Ok(cv)"
