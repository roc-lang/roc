app [main!] { pf: platform "./platform/main.roc" }

Page := [P].{
    send = |_page, payload| Json.to_str((payload, payload))
}
helper = |page, payload| page.send(payload)

main! : List(Str) => Try({}, [Exit(I8)])
main! = |_args| {
    truth : Bool
    truth = True
    a = helper(Page.P, "x") == "[\"x\",\"x\"]"
    b = helper(Page.P, truth) == "[true,true]"
    if a and b Ok({}) else Err(Exit(1))
}
