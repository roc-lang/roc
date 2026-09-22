app [main!] { pf: platform "./platform/main.roc", browser: "./browser/main.roc" }

import pf.Stdout
import browser.Browser

main! : List(Str) => Try({}, [Exit(I32), ..])
main! = |args| {
    url = match List.first(args) {
        Ok(a) => a
        Err(_) => "about:blank"
    }
    page = Browser.open({})
    match page.navigate(url) {
        Ok(json) => Stdout.line!(json)
        Err(_) => Stdout.line!("error")
    }
    Ok({})
}
