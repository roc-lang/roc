app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

# Test to verify record builder syntax works correctly
# Syntax: { field1: value1, field2: value2 }.TypeName
# Desugars to chained map2 calls:
# - 2 fields: TypeName.map2(v1, v2, |f1, f2| { f1, f2 })
# - 3 fields: TypeName.map2(v1, TypeName.map2(v2, v3, |f2, f3| (f2, f3)), |f1, (f2, f3)| { f1, f2, f3 })

# A simple Applicative type with ONLY map2 method
# The record builder chains map2 for 3+ fields
# map2 must be polymorphic to support chaining with tuple intermediates
Applicative := {}.{
    map2 : a, b, (a, b -> c) -> c
    map2 = |v1, v2, f| f(v1, v2)
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

    # Test 2: Three field record builder (chains map2)
    Stdout.line!("Test 2: Three field record builder (chains map2)")
    result2 = { a: 1, b: 2, c: 3 }.Applicative
    Stdout.line!("  { a: 1, b: 2, c: 3 }.Applicative")
    Stdout.line!("  Result: ${Str.inspect(result2)}")
    Stdout.line!("")

    # Test 3: Four field record builder (chains map2 twice)
    Stdout.line!("Test 3: Four field record builder (chains map2 twice)")
    result3 = { w: 10, x: 20, y: 30, z: 40 }.Applicative
    Stdout.line!("  { w: 10, x: 20, y: 30, z: 40 }.Applicative")
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
