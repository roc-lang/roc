import lib.W

## The opaque encoder carries a function-containing type beside its unique writer.
Fmt := [].{
    Held : { f : U64 -> U64 }
    Enc :: { writer : W.Writer, held : List(Held) }
    Items : { container : W.Container, held : List(Held) }

    Format :: {}.{
        rename_field : Format, Str -> Str
        rename_field = |_, name| name

        encode_u64 : U64, Enc -> Try(Enc, [])
        encode_u64 = |value, { writer: output, held }| W.put_u64(value, output).map_ok(|writer| Enc.{ writer, held })

        encode_list : Enc, U64, (Items, (Items, (Enc -> Try(Enc, err)) -> Try(Items, err)) -> Try(Items, err)) -> Try(Enc, err)
        encode_list = |{ writer: output, held }, count, write| W.list_carry(
            { writer: output, state: held },
            count,
            |items, add| write(
                { container: items.container, held: items.state },
                |item, write_value| add({ container: item.container, state: item.held }, |entry| write_value(Enc.{ writer: entry.writer, held: entry.state }).map_ok(|done| { writer: done.writer, state: done.held })).map_ok(|done| { container: done.container, held: done.state }),
            ).map_ok(|done| { container: done.container, state: done.held }),
        ).map_ok(|done| Enc.{ writer: done.writer, held: done.state })

        encode_record : Enc, U64, (Items, (Items, Str, (Enc -> Try(Enc, err)) -> Try(Items, err)) -> Try(Items, err)) -> Try(Enc, err)
        encode_record = |{ writer: output, held }, count, write| W.record_carry(
            { writer: output, state: held },
            count,
            |items, add| write(
                { container: items.container, held: items.state },
                |item, name, write_value| add({ container: item.container, state: item.held }, name, |entry| write_value(Enc.{ writer: entry.writer, held: entry.state }).map_ok(|done| { writer: done.writer, state: done.held })).map_ok(|done| { container: done.container, held: done.state }),
            ).map_ok(|done| { container: done.container, state: done.held }),
        ).map_ok(|done| Enc.{ writer: done.writer, held: done.state })
    }

    encode : a -> List(U8) where [a.encoder_for : Format -> (a, Enc -> Try(Enc, []))]
    encode = |value| {
        Shape : a
        match Shape.encoder_for(Format.{})(value, Enc.{ writer: W.new({}), held: [] }) {
            Ok({ writer, held: _ }) => W.bytes(writer)
        }
    }
}
