# repro for https://github.com/roc-lang/roc/issues/11868
#
# Destructuring a tuple whose element holds an unresolved type variable
# initializes that element's descriptor before any template reads it.
main! = |_| {
    (d, _) = (Dict.empty(), 0)
    _ = d
    (l, n) = ([], 0.U8)
    (s, t) = (Set.empty(), [[]])
    echo!("${List.len(l).to_str()} ${n.to_str()} ${Set.len(s).to_str()} ${List.len(t).to_str()}\n")
    Ok({})
}
