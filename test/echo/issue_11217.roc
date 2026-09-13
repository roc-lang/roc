main! = |args| {
    pair = |x| (args, x)
    alias = pair
    (first_capture, text) = alias("a long string captured through a polymorphic function alias")
    (second_capture, number) = pair(42)
    equal = |x, y| (args, x == y)
    eq = equal
    (third_capture, same_text) = eq(text, text)
    (fourth_capture, same_number) = eq(number, 42)

    if first_capture == args and second_capture == args and third_capture == args and fourth_capture == args and text == "a long string captured through a polymorphic function alias" and number == 42 and same_text and same_number {
        echo!("ok\n")
    } else {
        crash "incorrect polymorphic closure captures"
    }
    Ok({})
}
