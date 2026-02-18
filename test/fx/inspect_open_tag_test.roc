app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

# Test: using Str.inspect on tag unions
main! = || {
    # Test 1: Closed union inspects correctly using Str.inspect
    closed : [TagA, TagB, TagC]
    closed = TagB
    result1 = Str.inspect(closed)
    Stdout.line!("Closed: ${result1}")

    # Test 2: Tag with payload inspects correctly using Str.inspect
    with_payload : [Value(I32), Empty]
    with_payload = Value(42)
    result2 = Str.inspect(with_payload)
    Stdout.line!("With payload: ${result2}")

    # Test 3: Using Str.inspect on other types
    number = 123
    result3 = Str.inspect(number)
    Stdout.line!("Number: ${result3}")
}
