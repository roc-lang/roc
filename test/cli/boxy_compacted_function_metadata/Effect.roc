Effect(msg) := [Call(Str, List(U8), Box((List(U8) -> Box(msg))))].{
    call : Str, List(U8), (List(U8) -> msg) -> Effect(msg)
    call = |id, args, on_reply| Call(id, args, Box.box(|reply| Box.box(on_reply(reply))))

    reply : List(U8) -> Try(List(U8), [FeatureFailed(Str)])
    reply = |bytes|
        match bytes {
            [0, .. as answer] => Ok(answer)
            _ => Err(FeatureFailed("failed"))
        }

    fire : Effect(msg), List(U8) -> msg
    fire = |effect, reply|
        match effect {
            Call(_, _, callback) => Box.unbox(Box.unbox(callback)(reply))
        }
}

