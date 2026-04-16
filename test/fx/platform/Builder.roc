Builder := {
    value : Str,
    count : U64,
}.{
    new : Str -> Builder
    new = |val| {
        value: val,
        count: 0,
    }

    add_suffix : Builder, Str -> Builder
    add_suffix = |builder, suffix| {
        value: "${builder.value}${suffix}",
        count: builder.count + 1,
    }

    add_prefix : Builder, Str -> Builder
    add_prefix = |builder, prefix| {
        value: "${prefix}${builder.value}",
        count: builder.count + 1,
    }

    get_value : Builder -> Str
    get_value = |builder| builder.value

    # Anno-only effect method for testing static dispatch
    print_value! : Builder => {}
}
