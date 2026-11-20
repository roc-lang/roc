# repro for https://github.com/roc-lang/roc/issues/13279
Node := [Element({ tag : Str, rest : Str })]

is_clean : Node -> Bool
is_clean = |Element({ tag, .. })| !tag.is_empty()

rename : Node -> Node
rename = |Element({ rest, .. })| Element({ tag: "_", rest })

tag_of : Node -> Str
tag_of = |Element({ tag, .. })| tag

expect {
    tree = Element({ tag: "", rest: "" })
    [{}].all(|_| tag_of(if is_clean(tree) tree else rename(tree)) == "_")
}
