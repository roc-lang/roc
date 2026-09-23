app [main!] { pf: platform "./platform/main.roc" }

Page := [P].{
    send = |_page, payload| Json.to_str({ a: payload })
}

helper = |page| page.send("x")

main! : List(Str) => Try({}, [Exit(I8)])
main! = |_args|
    if helper(Page.P) == "{\"a\":\"x\"}" Ok({}) else Err(Exit(1))
