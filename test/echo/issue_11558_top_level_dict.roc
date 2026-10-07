# repro for https://github.com/roc-lang/roc/issues/11558
#
# A top-level `Dict` constant is restored into its use through `Dict`'s
# generic backing. Every backend, including `--specialize=no`, finds the key.
top_dict = Dict.single("a", "b")

main! = |args| {
    key = Str.concat("a", Str.join_with(args.drop_first(1), ""))
    echo!("${Str.inspect(top_dict.get(key))}\n")
    echo!("${Str.inspect(top_dict.get(Str.concat(key, "z")))}\n")
    Ok({})
}
