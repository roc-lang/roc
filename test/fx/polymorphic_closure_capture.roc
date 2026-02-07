app [main!] { pf: platform "./platform/main.roc" }
import pf.Stdout

use_closure = |value, f| {
    captured = |_x| value
    result = captured({})
    f(result)
}

main! = || {
    int_result = use_closure(42, |n| n.to_str())
    str_result = use_closure("hello", |s| s)
    Stdout.line!("int: ${int_result}")
    Stdout.line!("str: ${str_result}")
}
