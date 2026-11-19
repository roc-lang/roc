TryClosedBranchRewrite := {}

# Issue 10690: a closed `Try` beside a wrapped one in a `match` is rebuilt
# with `Ok(...?)`.

wrapped : (Str -> Try(Str, [FetchFailed(Str)])), Str -> Try(Str, _)
wrapped = |fetch, key| {
    v = fetch(key) ? Wrap
    Ok(v)
}

dispatch : (Str -> Try(Str, [FetchFailed(Str)])), Str -> Try(Str, _)
dispatch = |fetch, path|
    match path {
        "a" => Ok(fetch("x")?)
        _ => wrapped(fetch, "y")
    }

f : Str -> Try(Str, [FetchFailed(Str)])
f = |k| if k == "x" Err(FetchFailed(k)) else Ok(k)

expect match dispatch(f, "a") { Err(FetchFailed(_)) => Bool.True, _ => Bool.False }
expect match dispatch(f, "b") { Ok("y") => Bool.True, _ => Bool.False }
