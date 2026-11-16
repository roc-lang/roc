## A unique output buffer carried through container callbacks with caller state.
W := [].{
    Writer :: { out : List(U8), left : U64 }
    Container :: W.Writer
    WriterWith(s) : { writer : W.Writer, state : s }
    ContainerWith(s) : { container : W.Container, state : s }

    new : {} -> W.Writer
    new = |_| { out: [], left: 1 }

    bytes : W.Writer -> List(U8)
    bytes = |w| {
        { out, left: _ } = w
        out
    }

    put_u64 : U64, W.Writer -> Try(W.Writer, err)
    put_u64 = |value, w| {
        { out, left } = w
        Ok({ out: out.append(value.to_u8_wrap()), left })
    }

    list_carry : W.WriterWith(s), U64, (W.ContainerWith(s), (W.ContainerWith(s), (W.WriterWith(s) -> Try(W.WriterWith(s), err)) -> Try(W.ContainerWith(s), err)) -> Try(W.ContainerWith(s), err)) -> Try(W.WriterWith(s), err)
    list_carry = |e, count, write| nest_carry(e, 0x80, count, write, add_item_carry)

    record_carry : W.WriterWith(s), U64, (W.ContainerWith(s), (W.ContainerWith(s), Str, (W.WriterWith(s) -> Try(W.WriterWith(s), err)) -> Try(W.ContainerWith(s), err)) -> Try(W.ContainerWith(s), err)) -> Try(W.WriterWith(s), err)
    record_carry = |e, count, write| nest_carry(e, 0xa0, count, write, add_field_carry)
}

nest_carry : { writer : W.Writer, state : s }, U8, U64, ({ container : W.Container, state : s }, add -> Try({ container : W.Container, state : s }, err)), add -> Try({ writer : W.Writer, state : s }, err)
nest_carry = |e, head, count, write, add| {
    { writer, state } = e
    { out, left } = writer
    match write({ container: W.Container.({ out: out.append(head), left: count }), state }, add) {
        Ok({ container: W.Container.(inner), state: after }) => {
            { out: o, left: _ } = inner
            Ok({ writer: { out: o, left }, state: after })
        }
        Err(x) => Err(x)
    }
}

add_item_carry : { container : W.Container, state : s }, ({ writer : W.Writer, state : s } -> Try({ writer : W.Writer, state : s }, err)) -> Try({ container : W.Container, state : s }, err)
add_item_carry = |c, write_value| {
    { container: W.Container.(w), state } = c
    slot_carry(w, state, write_value).map_ok(|x| { container: W.Container.(x.writer), state: x.state })
}

add_field_carry : { container : W.Container, state : s }, Str, ({ writer : W.Writer, state : s } -> Try({ writer : W.Writer, state : s }, err)) -> Try({ container : W.Container, state : s }, err)
add_field_carry = |c, name, write_value| {
    { container: W.Container.(w), state } = c
    { out, left } = w
    slot_carry({ out: out.concat(name.to_utf8()), left }, state, write_value).map_ok(|x| { container: W.Container.(x.writer), state: x.state })
}

slot_carry : W.Writer, s, ({ writer : W.Writer, state : s } -> Try({ writer : W.Writer, state : s }, err)) -> Try({ writer : W.Writer, state : s }, err)
slot_carry = |w, state, write_value| {
    { out, left } = w
    write_value({ writer: { out, left: if (left == 0) 0 else left - 1 }, state })
}
