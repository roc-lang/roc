# Local procedures built from a declaration context whose requirements are
# called at a wider row than the implementation returns. Each function below
# (`make`) declares a type whose method a generated encoder calls, and its
# caller supplies that encoder, so the method's function value is built from
# the declaration context of the caller's instance of `make`.
#
# A method's own requirement takes its dictionary from the implementation,
# adapted to the wider row, and a method's body hands `make`'s dictionary on
# from its own worker's scope. When a generic caller of `make` hands `make`'s
# codec requirement on, the encoder is built by the caller's caller, from the
# instance of `make` that the caller's call of it instantiates: that
# instance's dictionary is the caller's, handed on from the caller's scope at
# the caller's narrower row, so it is adapted to the row `make` states. In
# every case `Ok` must not be read back under the wider row's numbering as
# `Extra`.
WidenClosedImplDeclarationContext := {}

Job := [Pending, Failed].{
    status : Job -> [Ok(Str), Err(Str)]
    status = |job| match job {
        Pending => Ok("cv")
        Failed => Err("broken")
    }
}

show : [Ok(Str), Err(Str), Extra] -> Str
show = |v| match v { Ok(s) => "Ok(${s})", Err(e) => "Err(${e})", Extra => "Extra" }

Logged : { item : Job, log : Str }

Count : { count : U64 }

# A format method with its own scheme, whose requirement states the wider
# row for its own variable. The encoder's state fixes that variable to `Job`.
own_requirement : a -> (Count, Logged -> Try(Logged, []))
own_requirement = |_x| {
    LocalFormat := [Default].{
        rename_field : LocalFormat, Str -> Str
        rename_field = |_, name| name

        encode_record : s, U64, (s, (s, Str, (s -> Try(s, [])) -> Try(s, [])) -> Try(s, [])) -> Try(s, [])
        encode_record = |state, _, write_fields| write_fields(state, |field_state, _name, write_value| write_value(field_state))

        encode_u64 : U64, { item : s, log : Str } -> Try({ item : s, log : Str }, [])
            where [s.status : s -> [Ok(Str), Err(Str), Extra]]
        encode_u64 = |_value, state| {
            empty : List(a)
            empty = []
            _unused = List.len(empty)
            Ok({ item: state.item, log: Str.concat(state.log, show(state.item.status())) })
        }
    }
    encoder_of : value -> (value, Logged -> Try(Logged, []))
        where [value.encoder_for : LocalFormat -> (value, Logged -> Try(Logged, []))]
    encoder_of = |_sample| {
        Shape : value
        |v, st| Shape.encoder_for(LocalFormat.Default)(v, st)
    }
    encoder_of({ count: 0.U64 })
}

TopFormat := [Default].{
    rename_field : TopFormat, Str -> Str
    rename_field = |_, name| name

    encode_record : s, U64, (s, (s, Str, (s -> Try(s, [])) -> Try(s, [])) -> Try(s, [])) -> Try(s, [])
    encode_record = |state, _, write_fields| write_fields(state, |field_state, _name, write_value| write_value(field_state))
}

# A custom encoder of a type holding `make`'s variable. Its signature names
# the type, so its scheme quantifies that variable with `make`'s own
# requirement, and its body reads the result at the wider row.
outer_requirement : a -> (Str -> Try(Str, []))
    where [a.status : a -> [Ok(Str), Err(Str)]]
outer_requirement = |x| {
    Inner := [In(a)].{
        encoder_for : TopFormat -> (Inner, Str -> Try(Str, []))
        encoder_for = |_format| |In(v), state| Ok(Str.concat(state, show(v.status())))
    }
    encoder_of : value -> (value, Str -> Try(Str, []))
        where [value.encoder_for : TopFormat -> (value, Str -> Try(Str, []))]
    encoder_of = |_sample| {
        Shape : value
        |v, st| Shape.encoder_for(TopFormat.Default)(v, st)
    }
    encoder = encoder_of({ inner: Inner.In(x) })
    |state| encoder({ inner: Inner.In(x) }, state)
}

apply = |f, v| f(v)

# The custom encoder hands `make`'s dictionary to a sibling local procedure
# whose own requirement states the wider row, called and used as a value.
sibling_requirement : a -> (Str -> Try(Str, []))
    where [a.status : a -> [Ok(Str), Err(Str)]]
sibling_requirement = |x| {
    widen : s -> [Ok(Str), Err(Str), Extra] where [s.status : s -> [Ok(Str), Err(Str), Extra]]
    widen = |y| {
        _unused : List(a)
        _unused = []
        y.status()
    }
    Inner := [In(a)].{
        encoder_for : TopFormat -> (Inner, Str -> Try(Str, []))
        encoder_for = |_format| |In(v), state| Ok(Str.concat(Str.concat(state, show(widen(v))), show(apply(widen, v))))
    }
    encoder_of : value -> (value, Str -> Try(Str, []))
        where [value.encoder_for : TopFormat -> (value, Str -> Try(Str, []))]
    encoder_of = |_sample| {
        Shape : value
        |v, st| Shape.encoder_for(TopFormat.Default)(v, st)
    }
    encoder = encoder_of({ inner: Inner.In(x) })
    |state| encoder({ inner: Inner.In(x) }, state)
}

Ticket := [Open].{
    sample : () -> Ticket
    sample = || Ticket.Open

    status : Ticket -> [Ok(Str), Err(Str)]
    status = |_| Ok("open")
}

Broken := [Broken].{
    sample : () -> Broken
    sample = || Broken.Broken

    status : Broken -> [Ok(Str), Err(Str)]
    status = |_| Err("broken")
}

Named : { name : Str }

# A format method reads `make`'s requirement through `make`'s scope, at the
# wider row `make` states.
wide_format : a -> (Named, Str -> Try(Str, []))
    where [a.sample : () -> a, a.status : a -> [Ok(Str), Err(Str), Extra]]
wide_format = |_x| {
    LocalFormat := [Default].{
        rename_field : LocalFormat, Str -> Str
        rename_field = |_, name| name

        encode_record : Str, U64, (Str, (Str, Str, (Str -> Try(Str, [])) -> Try(Str, [])) -> Try(Str, [])) -> Try(Str, [])
        encode_record = |state, _, write_fields| write_fields(state, |field_state, _name, write_value| write_value(field_state))

        encode_str : Str, Str -> Try(Str, [])
        encode_str = |value, state| {
            Outer : a
            made : a
            made = Outer.sample()
            Ok(Str.concat(Str.concat(state, value), show(made.status())))
        }
    }
    encoder_of : value -> (value, Str -> Try(Str, []))
        where [value.encoder_for : LocalFormat -> (value, Str -> Try(Str, []))]
    encoder_of = |_sample| {
        Shape : value
        |v, st| Shape.encoder_for(LocalFormat.Default)(v, st)
    }
    sample_name : Str
    sample_name = ""
    encoder_of({ name: sample_name })
}

# The caller states the narrower row and hands the codec requirement on.
narrow_caller : b -> (Named, Str -> Try(Str, []))
    where [b.sample : () -> b, b.status : b -> [Ok(Str), Err(Str)]]
narrow_caller = |x| wide_format(x)

log_of : Try(Logged, []) -> Str
log_of = |result| match result {
    Ok(state) => state.log
}

expect log_of(own_requirement(5.U64)({ count: 7 }, { item: Job.Pending, log: "" })) == "Ok(cv)"
expect log_of(own_requirement(5.U64)({ count: 7 }, { item: Job.Failed, log: "" })) == "Err(broken)"
expect outer_requirement(Job.Pending)("") == Ok("Ok(cv)")
expect outer_requirement(Job.Failed)("") == Ok("Err(broken)")
expect sibling_requirement(Job.Pending)("") == Ok("Ok(cv)Ok(cv)")
expect sibling_requirement(Job.Failed)("") == Ok("Err(broken)Err(broken)")
expect wide_format(Ticket.Open)({ name: "x" }, "") == Ok("xOk(open)")
expect narrow_caller(Ticket.Open)({ name: "x" }, "") == Ok("xOk(open)")
expect narrow_caller(Broken.Broken)({ name: "x" }, "") == Ok("xErr(broken)")
