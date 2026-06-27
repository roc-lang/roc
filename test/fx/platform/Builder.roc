Builder := {
    value : Str,
    count : U64,
    # Opt into declared-order layout so the host can mirror this struct's field
    # order (value before count). Without it, the record lays out structurally.
    _ : {},
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
