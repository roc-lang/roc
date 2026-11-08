import Effect

Media := [].{
    Problem : [NotAllowed, Other(Str)]

    play : Str, (Try({}, Media.Problem) -> msg) -> Effect(msg)
    play = |selector, on_done| Effect.call("joy/media/play", selector.to_utf8(), |reply| on_done(done(reply)))
}

done : List(U8) -> Try({}, Media.Problem)
done = |reply| Effect.reply(reply).map_ok(|_| {}).map_err(problem)

problem : [FeatureFailed(Str)] -> Media.Problem
problem = |FeatureFailed(reason)| Other(reason)

