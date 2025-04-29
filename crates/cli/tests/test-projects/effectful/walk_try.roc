app [main!] { pf: platform "../test-platform-effects-zig/main.roc" }

import pf.Effect

main! : {} => {}
main! = \{} ->
    # Sanity check for our test helper
    sanity_check = add_non_zero!(0, 1)
    expect sanity_check == Ok(1)

    # Test successful walk_try! with non-zero elements
    good = List.walk_try!([1, 2, 3], 0, \sum, num -> add_non_zero!(sum, num))
    expect good == Ok(6)

    # Test walk_try! with empty list
    good_empty = List.walk_try!([], 10, \sum, num -> add_non_zero!(sum, num))
    expect good_empty == Ok(10)

    # Test walk_try! that encounters a zero at different positions
    bad_a = List.walk_try!([1, 2, 0, 3], 0, \sum, num -> add_non_zero!(sum, num))
    expect bad_a == Err({})

    bad_b = List.walk_try!([0, 1, 2, 3], 0, \sum, num -> add_non_zero!(sum, num))
    expect bad_b == Err({})

    bad_c = List.walk_try!([1, 0, 2, 3], 0, \sum, num -> add_non_zero!(sum, num))
    expect bad_c == Err({})

    # Test more complex state accumulation
    # Accumulate pairs of [state, elem] but error on odd numbers
    complex_walk = List.walk_try!(
        [2, 4, 6, 8],
        [],
        \pairs, num ->
            if num % 2 == 0 then
                new_state = List.len(pairs) * 10
                result = Effect.id_effectful!(new_state)
                Ok(List.append(pairs, [result, num]))
            else
                _ = Effect.id_effectful!(num)
                Err({})
    )
    expect complex_walk == Ok([[0, 2], [10, 4], [20, 6], [30, 8]])

    # Test complex state walk that fails
    complex_walk_fail = List.walk_try!(
        [2, 4, 5, 8],
        [],
        \pairs, num ->
            if num % 2 == 0 then
                new_state = List.len(pairs) * 10
                result = Effect.id_effectful!(new_state)
                Ok(List.append(pairs, [result, num]))
            else
                _ = Effect.id_effectful!(num)
                Err({})
    )
    expect complex_walk_fail == Err({})

    {}

# Helper that returns Ok(sum) if num is non-zero, or Err({}) if num is zero
add_non_zero! : U64, U64 => Result U64 {}
add_non_zero! = \sum, num ->
    if num != 0 then
        Ok(sum + Effect.id_effectful!(num))
    else
        _ = Effect.id_effectful!(num)
        Err({})