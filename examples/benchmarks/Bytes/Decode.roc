interface Bytes.Decode exposes [ Decoder, decode, map, map2, u8, loop, Step, succeed, DecodeProblem, after, map3 ] imports []

State : { bytes : List U8, cursor : Nat }

DecodeProblem : [ OutOfBytes ]

Decoder a : [ @Decoder (State -> [ Good State a, Bad DecodeProblem ]) ]

decode : List U8, Decoder a -> Result a DecodeProblem
decode = \bytes, @Decoder decoder ->
    when decoder { bytes, cursor: 0 } is
        Good _ value ->
            Ok value

        Bad e ->
            Err e

succeed : a -> Decoder a
succeed = \value -> @Decoder \state -> Good state value

map : Decoder a, (a -> b) -> Decoder b
map = \@Decoder decoder, transform ->
    @Decoder
        \state ->
            when decoder state is
                Good state1 value ->
                    Good state1 (transform value)

                Bad e ->
                    Bad e

map2 : Decoder a, Decoder b, (a, b -> c) -> Decoder c
map2 = \@Decoder decoder1, @Decoder decoder2, transform ->
    @Decoder
        \state1 ->
            when decoder1 state1 is
                Good state2 a ->
                    when decoder2 state2 is
                        Good state3 b ->
                            Good state3 (transform a b)

                        Bad e ->
                            Bad e

                Bad e ->
                    Bad e

map3 : Decoder a, Decoder b, Decoder c, (a, b, c -> d) -> Decoder d
map3 = \@Decoder decoder1, @Decoder decoder2, @Decoder decoder3, transform ->
    @Decoder
        \state1 ->
            when decoder1 state1 is
                Good state2 a ->
                    when decoder2 state2 is
                        Good state3 b ->
                            when decoder3 state3 is
                                Good state4 c ->
                                    Good state4 (transform a b c)

                                Bad e ->
                                    Bad e

                        Bad e ->
                            Bad e

                Bad e ->
                    Bad e

after : Decoder a, (a -> Decoder b) -> Decoder b
after = \@Decoder decoder, transform ->
    @Decoder
        \state ->
            when decoder state is
                Good state1 value ->
                    (@Decoder decoder1) = transform value

                    decoder1 state1

                Bad e ->
                    Bad e

u8 : Decoder U8
u8 = @Decoder
    \state ->
        when List.get state.bytes state.cursor is
            Ok b ->
                Good { state & cursor: state.cursor + 1 } b

            Err _ ->
                Bad OutOfBytes

Step state b : [ Loop state, Done b ]

loop : (state -> Decoder (Step state a)), state -> Decoder a
loop = \stepper, initial ->
    @Decoder
        \state ->
            loopHelp stepper initial state

loopHelp = \stepper, accum, state ->
    (@Decoder stepper1) = stepper accum

    when stepper1 state is
        Good newState (Done value) ->
            Good newState value

        Good newState (Loop newAccum) ->
            loopHelp stepper newAccum newState

        Bad e ->
            Bad e
