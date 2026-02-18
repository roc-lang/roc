app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

main! = || {
    my_record = { name: "test", count: 42 }
    result = Str.inspect(my_record)
    Stdout.line!(result)
}
