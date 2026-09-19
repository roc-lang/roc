app [main!] { pf: platform "./static-lib-platform/main.roc" }

# `List.concat` increfs the elements it copies through a callback the caller
# passes in. Two element types give the builtin two different callbacks, so
# LLVM cannot fold the callback into a direct call and the wasm module keeps a
# `call_indirect`, which checks the callee's signature. The seed comes from the
# host, so none of this is evaluated at compile time.
#
# Every element is too long to be a small string, so each one is a heap
# allocation. The runner's allocation balance check then fails if an element
# callback is skipped or releases nothing.
main! = |seed| {
    tag = seed.to_str()

    strs = [tag.concat(" heap string number one")].concat([tag.concat(" heap string number two")])
    lists = [[tag.concat(" heap string number three")]].concat([[tag.concat(" heap string number four")]])

    joined = Str.join_with(strs.concat(lists.join()), ", ")

    if joined == "0 heap string number one, 0 heap string number two, 0 heap string number three, 0 heap string number four" {
        "ok"
    } else {
        "bad"
    }
}
