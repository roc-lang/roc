app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

# Minimal reproduction: List append crash with opaque type containing Str
MyRecord := { name : Str }.{}

main! = || {
    result_init : List(MyRecord)
    result_init = []
    var $result = result_init

    r1 : MyRecord
    r1 = { name: "first" }
    $result = List.append($result, r1)

    r2 : MyRecord
    r2 = { name: "second" }
    $result = List.append($result, r2)

    Stdout.line!("Done: ${List.len($result).to_str()}")
}
