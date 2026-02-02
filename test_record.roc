app [main!] { pf: platform "test/fx/platform/main.roc" }

import pf.Stdout

MyFile := { name : Str, content : Str }

main! : () => {}
main! = || {
    file : MyFile
    file = { name: "test.txt", content: "hello" }
    dbg file
    Stdout.line!("done")
}
