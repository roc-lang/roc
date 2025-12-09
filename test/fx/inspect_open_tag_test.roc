app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

# Test: using Str.inspekt on tag unions
main! = || {
    # Test 1: Closed union inspects correctly using Str.inspekt
    closed : [TagA, TagB, TagC]
    closed = TagB
    result1 = Str.inspekt(closed)
    Stdout.line!("Closed: ${result1}")

    # Test 2: Tag with payload inspects correctly using Str.inspekt
    with_payload : [Value(I32), Empty]
    with_payload = Value(42)
    result2 = Str.inspekt(with_payload)
    Stdout.line!("With payload: ${result2}")

    # Test 3: Using Str.inspekt on other types
    number = 123
    result3 = Str.inspekt(number)
    Stdout.line!("Number: ${result3}")
}
