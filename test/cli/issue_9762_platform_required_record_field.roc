app [main!] { pf: platform "./issue_9762_record_platform/main.roc" }

import pf.Echo

main! = |config| {
    Echo.line!(config.name.not_a_method())

    Ok({})
}
