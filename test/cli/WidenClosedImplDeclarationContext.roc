# Local procedures built from a declaration context whose requirements are
# called at a wider row than the implementation returns. Each `make` below
# declares a type whose method a generated encoder calls, and its caller
# supplies that encoder, so the method's function value is built from the
# declaration context of the caller's instance of `make`.
#
# No evidence read there names a dictionary of `make`'s scope: the encoder's
# contract is validated where `make`'s variable is a closed type, so the
# method's own requirement takes its dictionary from the implementation,
# adapted to the wider row, and the method's body hands `make`'s dictionary
# on from its own worker's scope. Either way `Ok` must not be read back under
# the wider row's numbering as `Extra`.
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
