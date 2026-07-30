# Support module for nominal_callable_specializations.roc: a nominal record
# holding a function-backed nominal, so specializations built from
# `default()` alone carry a narrower callable member set than ones built
# through `register`, behind one public type.
import EchoHandler

EchoFrame(state) := {
    handler : EchoHandler(state),
}.{
    default : () -> EchoFrame(state)
    default = || { handler: EchoHandler.default() }

    register : EchoHandler(state), EchoFrame(state) -> EchoFrame(state)
    register = |handler, frame| { handler: frame.handler + handler }

    run! : List(U64), U64, EchoFrame(U64) => U64
    run! = |items, start, frame| {
        var $acc = start
        for item in items {
            $acc = match (frame.handler).dispatch!($acc + item) {
                Handled(next) => next
                Declined => $acc
            }
        }
        $acc
    }
}
