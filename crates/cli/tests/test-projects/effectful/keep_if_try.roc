app [main!] { pf: platform "../test-platform-effects-zig/main.roc" }

import pf.Effect

main! : {} => {}
main! = \{} ->
    when run!({}) is
        Ok({}) ->
            Effect.put_line!("Success!")
        Err(err) ->
            crash "Error: ${Inspect.to_str(err)}"

#run! : {} => Result {} _
run! = \{} ->
    sanity_check_bool = is_zero!(0)?
    err_on_false(sanity_check_bool, "sanity check")?

    two_zeros = List.keep_if_try!([1, 0, 0], is_zero!)?
    err_on_false((two_zeros == [0, 0]), "test 1")?

    one_zero = List.keep_if_try!([0, 1], is_zero!)?
    err_on_false((one_zero == [0]), "test 2")?

    one_zero_again = List.keep_if_try!([1, 0], is_zero!)?
    err_on_false((one_zero_again == [0]), "test 3")?

    three_zeros = List.keep_if_try!([0, 0, 0], is_zero!)?
    err_on_false((three_zeros == [0, 0, 0]), "test 4")?

    just_one = List.keep_if_try!([1], is_zero!)?
    err_on_false((just_one == []), "test 5")?

    one_zero_input = List.keep_if_try!([0], is_zero!)?
    err_on_false((one_zero_input == [0]), "test 6")?

    empty_list = List.keep_if_try!([], is_zero!)?
    err_on_false((empty_list == []), "test 7")?

    Ok({})

is_zero! : U64 => Result Bool _
is_zero! = \num ->
    _hey = Effect.id_effectful!(num)
    if num == 0 then
        Ok(Bool.true)
    else if num == 1 then
        Ok(Bool.false)
    else
        Err(NotZeroOrOne)

err_on_false : Bool, Str -> Result {} _
err_on_false = |bool, test_name|
    if bool then
        Ok({})
    else
        Err(StrErr("Test failed: ${Inspect.to_str(test_name)}"))