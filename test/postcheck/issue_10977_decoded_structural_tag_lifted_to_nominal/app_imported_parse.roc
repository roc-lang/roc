app [main!] { pf: platform "../../fx-open/platform/main.roc" }

import pf.Stdout
import Bar
import Parse

main! = |_args| {
    match Parse.parse_fuzz("{\"inner\":{\"alpha\":\"a\",\"beta\":\"Baz\"}}") {
        Ok(fuzz) => Stdout.line!(Bar.frob(fuzz))
        Err(_) => Stdout.line!("failed")
    }
    Ok({})
}
