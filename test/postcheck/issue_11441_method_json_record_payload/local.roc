app [main!] { pf: platform "./platform/main.roc" }

Page := [P].{
    send = |_page, payload| Json.to_str({ a: payload })
}

main! : List(Str) => Try({}, [Exit(I8)])
main! = |_args| {
    helper = |page| page.send("x")
    if helper(Page.P) == "{\"a\":\"x\"}" Ok({}) else Err(Exit(1))
}
