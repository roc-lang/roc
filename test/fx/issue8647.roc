app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

main! = || {
    my_record = { name: "test" }
    # Test field access (was causing TypeMismatch)
    Stdout.line!(my_record.name)
}
