Issue12948CrashingExpect := [].{}

# `roc test` on the dev backend reports a crashing `expect` as a failed test
# and keeps running the others.

expect {
    crash "boom"
}

expect {
    dbg Json.to_str({ f64: 0.1.F64 })
    True
}

expect 1 + 1 == 2
