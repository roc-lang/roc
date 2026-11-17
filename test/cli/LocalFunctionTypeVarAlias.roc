# Local functions whose only reference to an enclosing function's type
# variable is a type-rooted call through a type variable alias (`Outer : a`).
# The call reads the variable's requirement from the enclosing function, so
# the local function depends on that function and stays one of its local
# procedures; promoted to a procedure of its own, its call has no requirement
# to read and crashes as a dispatch on a value that can never exist.
LocalFunctionTypeVarAlias := {}

Apples := [Apples].{
    label : () -> Str
    label = || "apples"

    sample : () -> Apples
    sample = || Apples.Apples

    name : Apples -> Str
    name = |_| "apple"
}

# The alias is declared inside the local function.
inside_alias : a -> Str where [a.label : () -> Str]
inside_alias = |_x| {
    g = |n| {
        Outer : a
        Str.concat(Outer.label(), n)
    }
    g("!")
}

# The alias is declared in the enclosing body.
outside_alias : a -> Str where [a.label : () -> Str]
outside_alias = |_x| {
    Outer : a
    g = |n| Str.concat(Outer.label(), n)
    g("!")
}

# The local function's annotation names no variable.
annotated : a -> Str where [a.label : () -> Str]
annotated = |_x| {
    g : Str -> Str
    g = |n| {
        Outer : a
        Str.concat(Outer.label(), n)
    }
    g("!")
}

# The call's result is the variable, dispatched on in turn.
result_dispatched : a -> Str where [a.sample : () -> a, a.name : a -> Str]
result_dispatched = |_x| {
    g = |n| {
        Outer : a
        Str.concat(Outer.sample().name(), n)
    }
    g("!")
}

Named : { name : Str }

# A format method of a type declared in the body, whose signature names
# neither the type nor the variable.
format_method : a -> (Named, Str -> Try(Str, [])) where [a.label : () -> Str]
format_method = |_x| {
    LocalFormat := [Default].{
        rename_field : LocalFormat, Str -> Str
        rename_field = |_, name| name

        encode_record : Str, U64, (Str, (Str, Str, (Str -> Try(Str, [])) -> Try(Str, [])) -> Try(Str, [])) -> Try(Str, [])
        encode_record = |state, _, write_fields| write_fields(state, |field_state, _name, write_value| write_value(field_state))

        encode_str : Str, Str -> Try(Str, [])
        encode_str = |value, state| {
            Outer : a
            Ok(Str.concat(Str.concat(state, value), Outer.label()))
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

expect inside_alias(Apples.Apples) == "apples!"
expect outside_alias(Apples.Apples) == "apples!"
expect annotated(Apples.Apples) == "apples!"
expect result_dispatched(Apples.Apples) == "apple!"
expect format_method(Apples.Apples)({ name: "x" }, "") == Ok("xapples")
