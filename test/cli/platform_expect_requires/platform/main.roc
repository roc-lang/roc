platform ""
    requires {
        answer : I64
    }
    exposes []
    packages {}
    provides { "roc_main": main_for_host }

main_for_host : {} -> I64
main_for_host = |_| helper({})

helper = |_| answer + 1

derived = answer + 2

fn_alias = helper

plain : I64
plain = 40

# `roc test` pairs no platform with an app, so only the first expect can run.
expect plain + 2 == 42

expect answer == 42

expect helper({}) == 43

expect derived == 44

expect fn_alias({}) == 43

expect {
    local = |x| x + answer
    local(1) == 43
}
