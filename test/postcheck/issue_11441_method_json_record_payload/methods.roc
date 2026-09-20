app [main!] { pf: platform "./platform/main.roc" }

Store := [S].{
    load = |_store| "loaded"
    consume = |_store, text| text == "loaded"
}
Page := [P].{
    send = |_page, payload, store| {
        loaded = store.load()
        if store.consume(loaded) Json.to_str({ a: payload }) else "bad load"
    }
}
helper = |page, payload, store| page.send(payload, store)

main! : List(Str) => Try({}, [Exit(I8)])
main! = |_args|
    if helper(Page.P, "x", Store.S) == "{\"a\":\"x\"}" Ok({}) else Err(Exit(1))
