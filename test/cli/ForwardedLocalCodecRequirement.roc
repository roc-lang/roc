# A function that declares a type whose methods a generated codec calls, and
# builds that codec from its own scheme's captured codec requirement, called
# by a generic function. The requirement's relation names the caller's type
# variable, so the caller's scheme captures it too and hands it on: the codec
# is built by the caller's caller, whose dictionaries for the caller are the
# only instance in reach. Under `.boxy` the codec's local procedures are built
# from the declaring function's instance that the caller's call of it
# instantiates, which checking records as the edge handing the requirement on.
ForwardedLocalCodecRequirement := {}

Job := [Pending, Failed].{
    status : Job -> [Ok(Str), Err(Str)]
    status = |job| match job {
        Pending => Ok("cv")
        Failed => Err("broken")
    }

    label : Job -> Str
    label = |_| "job"
}

Only := [One].{
    status : Only -> [Ok(Str)]
    status = |_| Ok("only")

    label : Only -> Str
    label = |_| "only"
}

show : [Ok(Str), Err(Str), Extra] -> Str
show = |v| match v { Ok(s) => "Ok(${s})", Err(e) => "Err(${e})", Extra => "Extra" }

Count : { count : U64 }

TopFormat := [Default].{
    rename_field : TopFormat, Str -> Str
    rename_field = |_, name| name

    encode_record : s, U64, (s, (s, Str, (s -> Try(s, [])) -> Try(s, [])) -> Try(s, [])) -> Try(s, [])
    encode_record = |state, _, write_fields| write_fields(state, |field_state, _name, write_value| write_value(field_state))
}

# A format declared in the body, whose backing holds the body's variable and
# whose method reads it.
with_format : a -> (Count, Str -> Try(Str, []))
    where [a.status : a -> [Ok(Str), Err(Str)]]
with_format = |x| {
    LocalFormat := [Default(a)].{
        rename_field : LocalFormat, Str -> Str
        rename_field = |_, name| name

        encode_record : s, U64, (s, (s, Str, (s -> Try(s, [])) -> Try(s, [])) -> Try(s, [])) -> Try(s, [])
        encode_record = |state, _, write_fields| write_fields(state, |field_state, _name, write_value| write_value(field_state))

        encode_u64 : U64, Str -> Try(Str, [])
        encode_u64 = |value, state| {
            _unused : List(a)
            _unused = []
            Ok(Str.concat(state, value.to_str()))
        }
    }
    encoder_of : value -> (value, Str -> Try(Str, []))
        where [value.encoder_for : LocalFormat -> (value, Str -> Try(Str, []))]
    encoder_of = |_sample| {
        Shape : value
        |v, st| Shape.encoder_for(LocalFormat.Default(x))(v, st)
    }
    encoder_of({ count: 0.U64 })
}

forward_format : b -> (Count, Str -> Try(Str, []))
    where [b.status : b -> [Ok(Str), Err(Str)]]
forward_format = |x| with_format(x)

# A type declared in the body holding the body's variable, whose custom
# encoder dispatches on it through the body's where clause.
with_custom : a -> (Str -> Try(Str, []))
    where [a.status : a -> [Ok(Str), Err(Str)]]
with_custom = |x| {
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

forward_custom : b -> (Str -> Try(Str, []))
    where [b.status : b -> [Ok(Str), Err(Str)]]
forward_custom = |x| with_custom(x)

# The caller has a requirement of its own, ordered before the forwarded
# one, and uses it.
forward_with_own : b -> (Str -> Try(Str, []))
    where [b.label : b -> Str, b.status : b -> [Ok(Str), Err(Str)]]
forward_with_own = |x| {
    inner = with_custom(x)
    |state| inner(Str.concat(state, x.label()))
}

# The encoder calls a sibling local procedure whose own requirement states a
# wider row, and the caller states a narrower one.
with_sibling : a -> (Str -> Try(Str, []))
    where [a.status : a -> [Ok(Str), Err(Str)]]
with_sibling = |x| {
    widen : s -> [Ok(Str), Err(Str), Extra] where [s.status : s -> [Ok(Str), Err(Str), Extra]]
    widen = |y| {
        _unused : List(a)
        _unused = []
        y.status()
    }
    Inner := [In(a)].{
        encoder_for : TopFormat -> (Inner, Str -> Try(Str, []))
        encoder_for = |_format| |In(v), state| Ok(Str.concat(state, show(widen(v))))
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

forward_sibling : b -> (Str -> Try(Str, []))
    where [b.status : b -> [Ok(Str)]]
forward_sibling = |x| with_sibling(x)

# The requirement is handed on twice.
forward_twice : c -> (Str -> Try(Str, []))
    where [c.status : c -> [Ok(Str), Err(Str)]]
forward_twice = |x| forward_custom(x)

# The forwarding call sits in a local function of the caller's body,
# capturing the caller's value, whose scope captures the requirement in turn.
via_closure : b -> (Str -> Try(Str, []))
    where [b.status : b -> [Ok(Str), Err(Str)]]
via_closure = |x| {
    go = |_n| with_custom(x)
    go({})
}

# The local function is generic in a variable of its own, used at two types.
via_closure_generic : b -> (Str -> Try(Str, []))
    where [b.status : b -> [Ok(Str), Err(Str)]]
via_closure_generic = |x| {
    go = |n| {
        _ignored = n
        with_custom(x)
    }
    _other = go("s")
    go(1.U8)
}

# The forwarding call sits in a generalized local function of its own
# variable, which captures the requirement over that variable.
via_generic_local : b -> (Str -> Try(Str, []))
    where [b.status : b -> [Ok(Str), Err(Str)]]
via_generic_local = |x| {
    go = |y| with_custom(y)
    go(x)
}

# The forwarding use names a procedure alias, top-level and local.
fwd : a -> (Str -> Try(Str, [])) where [a.status : a -> [Ok(Str), Err(Str)]]
fwd = with_custom

via_alias : b -> (Str -> Try(Str, []))
    where [b.status : b -> [Ok(Str), Err(Str)]]
via_alias = |x| fwd(x)

via_local_alias : b -> (Str -> Try(Str, []))
    where [b.status : b -> [Ok(Str), Err(Str)]]
via_local_alias = |x| {
    local_fwd : c -> (Str -> Try(Str, [])) where [c.status : c -> [Ok(Str), Err(Str)]]
    local_fwd = with_custom
    local_fwd(x)
}

# The same aliases unannotated: each has its scheme and its captured
# requirement from the procedure it names.
fwd_plain = with_custom

via_plain_alias : b -> (Str -> Try(Str, []))
    where [b.status : b -> [Ok(Str), Err(Str)]]
via_plain_alias = |x| fwd_plain(x)

via_plain_local_alias : b -> (Str -> Try(Str, []))
    where [b.status : b -> [Ok(Str), Err(Str)]]
via_plain_local_alias = |x| {
    local_fwd = with_custom
    local_fwd(x)
}

# The forwarding edge is a dispatch, of a method and of a method bound to an
# alias, directly and through a dictionary.
Maker(a) := [Maker(a)].{
    build : Maker(a) -> (Str -> Try(Str, [])) where [a.status : a -> [Ok(Str), Err(Str)]]
    build = |Maker.Maker(x)| with_custom(x)

    built : Maker(a) -> (Str -> Try(Str, [])) where [a.status : a -> [Ok(Str), Err(Str)]]
    built = build
}

via_method : b -> (Str -> Try(Str, []))
    where [b.status : b -> [Ok(Str), Err(Str)]]
via_method = |x| Maker.Maker(x).build()

via_method_alias : b -> (Str -> Try(Str, []))
    where [b.status : b -> [Ok(Str), Err(Str)]]
via_method_alias = |x| Maker.Maker(x).built()

through_dict : m -> (Str -> Try(Str, [])) where [m.built : m -> (Str -> Try(Str, []))]
through_dict = |maker| maker.built()

via_method_alias_dict : b -> (Str -> Try(Str, []))
    where [b.status : b -> [Ok(Str), Err(Str)]]
via_method_alias_dict = |x| through_dict(Maker.Maker(x))

expect forward_format(Job.Pending)({ count: 7 }, "n") == Ok("n7")
expect forward_custom(Job.Pending)("") == Ok("Ok(cv)")
expect forward_custom(Job.Failed)("") == Ok("Err(broken)")
expect forward_with_own(Job.Pending)("") == Ok("jobOk(cv)")
expect forward_with_own(Job.Failed)("") == Ok("jobErr(broken)")
expect with_sibling(Job.Pending)("") == Ok("Ok(cv)")
expect forward_sibling(Only.One)("") == Ok("Ok(only)")
expect forward_twice(Job.Pending)("") == Ok("Ok(cv)")
expect forward_twice(Job.Failed)("") == Ok("Err(broken)")
expect via_closure(Job.Pending)("") == Ok("Ok(cv)")
expect via_closure_generic(Job.Failed)("") == Ok("Err(broken)")
expect via_alias(Job.Pending)("") == Ok("Ok(cv)")
expect via_local_alias(Job.Failed)("") == Ok("Err(broken)")
expect via_method(Job.Pending)("") == Ok("Ok(cv)")
expect via_method_alias(Job.Failed)("") == Ok("Err(broken)")
expect via_method_alias_dict(Job.Pending)("") == Ok("Ok(cv)")
expect via_generic_local(Job.Pending)("") == Ok("Ok(cv)")
expect via_plain_alias(Job.Failed)("") == Ok("Err(broken)")
expect via_plain_local_alias(Job.Pending)("") == Ok("Ok(cv)")
expect fwd_plain(Job.Pending)("") == Ok("Ok(cv)")
expect fwd(Job.Failed)("") == Ok("Err(broken)")
expect {
    go = |y| with_custom(y)
    go(Job.Failed)("") == Ok("Err(broken)")
}
