module [ByteDecoder, decode, map, map2, u8, loop, Step, succeed, DecodeProblem, after, map3]

State : { bytes : List U8, cursor : U64 }

DecodeProblem : [OutOfBytes]

ByteDecoder a := State -> [Good State a, Bad DecodeProblem]

decode : List U8, ByteDecoder a -> Result a DecodeProblem
decode = \bytes, @ByteDecoder(decoder) ->
    when decoder({ bytes, cursor: 0 }) is
        Good(_, value) ->
            Ok(value)

        Bad(e) ->
            Err(e)

succeed : a -> ByteDecoder a
succeed = \value -> @ByteDecoder(\state -> Good(state, value))

map : ByteDecoder a, (a -> b) -> ByteDecoder b
map = \@ByteDecoder(decoder), transform ->
    @ByteDecoder(
        \state ->
            when decoder(state) is
                Good(state1, value) ->
                    Good(state1, transform(value))

                Bad(e) ->
                    Bad(e),
    )

map2 : ByteDecoder a, ByteDecoder b, (a, b -> c) -> ByteDecoder c
map2 = \@ByteDecoder(decoder1), @ByteDecoder(decoder2), transform ->
    @ByteDecoder(
        \state1 ->
            when decoder1(state1) is
                Good(state2, a) ->
                    when decoder2(state2) is
                        Good(state3, b) ->
                            Good(state3, transform(a, b))

                        Bad(e) ->
                            Bad(e)

                Bad(e) ->
                    Bad(e),
    )

map3 : ByteDecoder a, ByteDecoder b, ByteDecoder c, (a, b, c -> d) -> ByteDecoder d
map3 = \@ByteDecoder(decoder1), @ByteDecoder(decoder2), @ByteDecoder(decoder3), transform ->
    @ByteDecoder(
        \state1 ->
            when decoder1(state1) is
                Good(state2, a) ->
                    when decoder2(state2) is
                        Good(state3, b) ->
                            when decoder3(state3) is
                                Good(state4, c) ->
                                    Good(state4, transform(a, b, c))

                                Bad(e) ->
                                    Bad(e)

                        Bad(e) ->
                            Bad(e)

                Bad(e) ->
                    Bad(e),
    )

after : ByteDecoder a, (a -> ByteDecoder b) -> ByteDecoder b
after = \@ByteDecoder(decoder), transform ->
    @ByteDecoder(
        \state ->
            when decoder(state) is
                Good(state1, value) ->
                    @ByteDecoder(decoder1) = transform(value)

                    decoder1(state1)

                Bad(e) ->
                    Bad(e),
    )

u8 : ByteDecoder U8
u8 = @ByteDecoder(
    \state ->
        when List.get(state.bytes, state.cursor) is
            Ok(b) ->
                Good({ state & cursor: state.cursor + 1 }, b)

            Err(_) ->
                Bad(OutOfBytes),
)

Step state b : [Loop state, Done b]

loop : (state -> ByteDecoder (Step state a)), state -> ByteDecoder a
loop = \stepper, initial ->
    @ByteDecoder(
        \state ->
            loop_help(stepper, initial, state),
    )

loop_help = \stepper, accum, state ->
    @ByteDecoder(stepper1) = stepper(accum)

    when stepper1(state) is
        Good(new_state, Done(value)) ->
            Good(new_state, value)

        Good(new_state, Loop(new_accum)) ->
            loop_help(stepper, new_accum, new_state)

        Bad(e) ->
            Bad(e)
