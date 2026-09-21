app [main!] { pf: platform "./platform/main.roc" }

Page := [P].{
    receive = |_page, text, fallback| match Json.parse(text) {
        Ok(value) => value
        Err(_) => { a: fallback }
    }
}
helper = |page, text, fallback| page.receive(text, fallback)

main! : List(Str) => Try({}, [Exit(I8)])
main! = |_args| {
    fallback : Bool
    fallback = False
    a = helper(Page.P, "{\"a\":\"x\"}", "fallback").a == "x"
    b = helper(Page.P, "bad json", "fallback").a == "fallback"
    c = helper(Page.P, "{\"a\":true}", fallback).a == True
    if a and b and c Ok({}) else Err(Exit(1))
}
