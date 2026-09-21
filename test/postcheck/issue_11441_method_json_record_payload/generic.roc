app [main!] { pf: platform "./platform/main.roc" }

Page := [P].{
    send = |_page, payload| Json.to_str({ a: payload })
}
helper = |page, payload| page.send(payload)
forward = |page, payload| helper(page, payload)

main! : List(Str) => Try({}, [Exit(I8)])
main! = |_args| {
    truth : Bool
    truth = True
    a = forward(Page.P, "x") == "{\"a\":\"x\"}"
    b = forward(Page.P, truth) == "{\"a\":true}"
    c = forward(Page.P, { b: "y" }) == "{\"a\":{\"b\":\"y\"}}"
    if a and b and c Ok({}) else Err(Exit(1))
}
