# Support module for nominal_callable_specializations.roc: a nominal type
# wrapping a function, with a combinator whose closure captures two handlers,
# making the callable's lambda set recursive through its captures.

EchoHandler(state) := (state => [Handled(state), Declined]).{
    default : () -> EchoHandler(state)
    default = || EchoHandler.(|_state| Declined)

    plus : EchoHandler(state), EchoHandler(state) -> EchoHandler(state)
    plus = |EchoHandler.(earlier!), EchoHandler.(later!)| {
        EchoHandler.(
            |state| match later!(state) {
                Handled(next) => Handled(next)
                Declined => earlier!(state)
            },
        )
    }

    dispatch! : EchoHandler(state), state => [Handled(state), Declined]
    dispatch! = |EchoHandler.(handle!), state| handle!(state)
}
