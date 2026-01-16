app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

# Test to verify record builder syntax works correctly
# Syntax: { field1: value1, field2: value2 }.TypeName
# Desugars to: TypeName.mapN(value1, value2, |field1, field2| { field1, field2 })

# A simple Applicative type with map and map2 methods
Applicative := {}.{
    # Single field builder (map for 1 field)
    map : I64, (I64 -> a) -> a
    map = |v, f| f(v)

    # Two field builder (map2 for 2 fields)
    map2 : I64, I64, (I64, I64 -> a) -> a
    map2 = |v1, v2, f| f(v1, v2)

    # Three field builder (map3 for 3 fields)
    map3 : I64, I64, I64, (I64, I64, I64 -> a) -> a
    map3 = |v1, v2, v3, f| f(v1, v2, v3)
}

main! = || {
    Stdout.line!("=== Record Builder Syntax Tests ===")
    Stdout.line!("")

    # Test 1: Two field record builder
    Stdout.line!("Test 1: Two field record builder")
    result1 = { x: 10, y: 20 }.Applicative
    Stdout.line!("  { x: 10, y: 20 }.Applicative")
    Stdout.line!("  Result: ${Str.inspect(result1)}")
    Stdout.line!("")

    # Test 2: Single field record builder
    Stdout.line!("Test 2: Single field record builder")
    result2 = { value: 42 }.Applicative
    Stdout.line!("  { value: 42 }.Applicative")
    Stdout.line!("  Result: ${Str.inspect(result2)}")
    Stdout.line!("")

    # Test 3: Three field record builder
    Stdout.line!("Test 3: Three field record builder")
    result3 = { a: 1, b: 2, c: 3 }.Applicative
    Stdout.line!("  { a: 1, b: 2, c: 3 }.Applicative")
    Stdout.line!("  Result: ${Str.inspect(result3)}")
    Stdout.line!("")

    # Test 4: Using variables in field values
    Stdout.line!("Test 4: Using variables in field values")
    x = 100
    y = 200
    result4 = { first: x, second: y }.Applicative
    Stdout.line!("  x = 100, y = 200")
    Stdout.line!("  { first: x, second: y }.Applicative")
    Stdout.line!("  Result: ${Str.inspect(result4)}")
    Stdout.line!("")

    # Test 5: Verify equivalence with direct call
    Stdout.line!("Test 5: Equivalence with direct call")
    builder_result = { p: 5, q: 10 }.Applicative
    direct_result = Applicative.map2(5, 10, |p, q| { p, q })
    Stdout.line!("  Record builder: ${Str.inspect(builder_result)}")
    Stdout.line!("  Direct call:    ${Str.inspect(direct_result)}")
    Stdout.line!("")

    Stdout.line!("=== All tests completed! ===")
}
