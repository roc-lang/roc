import Inner

Wrap := [W].{
    encoder_for = |encoding| {
        encode_inner = Inner.encoder_for(encoding)
        |W, state| encode_inner(Inner.On, state)
    }

    parser_for = |format| {
        parse_inner = Inner.parser_for(format)
        |state| {
            parsed = parse_inner(state)?
            match parsed.value {
                Inner.On => Ok({ value: W, rest: parsed.rest })
                Inner.Off => Ok({ value: W, rest: parsed.rest })
            }
        }
    }
}
