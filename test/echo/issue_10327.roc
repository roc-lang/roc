# Repro for https://github.com/roc-lang/roc/issues/10327.
# This declaration order is significant. The nested `next` binder shadows the
# later top-level declaration, so canonicalization must report that diagnostic
# instead of corrupting the lookup identity and panicking in postcheck.
Message : [AuthOk, Other]

connect! : {} => Try(U64, _)
connect! = |_|
    message_loop!(
        0,
        |msg, state| match msg {
            AuthOk => next(state)
            _ => Ok(Done(99))
        },
    )

loop! : state, (state => Try([Step(state), Done(done)], err)) => Try(done, err)
loop! = |state, fn!| match fn!(state) {
    Err(err) => Err(err)
    Ok(Done(done)) => Ok(done)
    Ok(Step(next)) => loop!(next, fn!)
}

message_loop! : state, (Message, state => Try([Done(done), Step(state)], _)) => Try(done, _)
message_loop! = |init_state, step_fn!|
    loop!(
        init_state,
        |state| match state {
            _ => step_fn!(Other, state)
        },
    )

next : a -> Try([Step(a), ..], _)
next = |state| Ok(Step(state))

main! = |_args| {
    _client = connect!({})?
    Ok({})
}
