app [main!] { pf: platform "./static-lib-platform/main.roc" }

u8_at : List(U8), U64, U8 -> Bool
u8_at = |list, index, expected|
    match List.get(list, index) {
        Ok(value) => value == expected
        Err(_) => False
    }

str_byte_count_at : List(Str), U64, U64 -> Bool
str_byte_count_at = |list, index, expected|
    match List.get(list, index) {
        Ok(value) => Str.count_utf8_bytes(value) == expected
        Err(_) => False
    }

sound_at : List((Str, U32, U32)), U64, Str, U32, U32 -> Bool
sound_at = |list, index, expected_label, expected_pitch, expected_duration|
    match List.get(list, index) {
        Ok((label, pitch, duration)) =>
            label == expected_label and pitch == expected_pitch and duration == expected_duration
        Err(_) => False
    }

set_str_or_empty : List(Str), U64, Str -> List(Str)
set_str_or_empty = |list, index, replacement|
    match List.set(list, index, replacement) {
        Ok(result) => result
        Err(_) => []
    }

set_sound_or_empty : List((Str, U32, U32)), U64, (Str, U32, U32) -> List((Str, U32, U32))
set_sound_or_empty = |list, index, replacement|
    match List.set(list, index, replacement) {
        Ok(result) => result
        Err(_) => []
    }

set_zst_or_empty : List({}), U64, {} -> List({})
set_zst_or_empty = |list, index, replacement|
    match List.set(list, index, replacement) {
        Ok(result) => result
        Err(_) => []
    }

flat_list_ops_ok : U64 -> Bool
flat_list_ops_ok = |seed| {
    first = if seed == 0 { 1.U8 } else { 7.U8 }
    appended = List.append([first, 2.U8, 3.U8], 4.U8)
    concatted = List.concat(appended, [5.U8, 6.U8])
    dropped = List.drop_at(concatted, 2)
    reversed = List.rev(dropped)

    List.len(reversed) == 5
        and u8_at(reversed, 0, 6.U8)
        and u8_at(reversed, 1, 5.U8)
        and u8_at(reversed, 2, 4.U8)
        and u8_at(reversed, 3, 2.U8)
        and u8_at(reversed, 4, first)
}

refcounted_list_ops_ok : Bool
refcounted_list_ops_ok = {
    concatted = List.concat(["a", "bb"], ["ccc"])
    dropped = List.drop_at(concatted, 1)

    List.len(dropped) == 2
        and str_byte_count_at(dropped, 0, 1)
        and str_byte_count_at(dropped, 1, 3)
}

refcounted_list_set_ok : Bool
refcounted_list_set_ok = {
    source = ["left", "middle", "right"]
    updated = set_str_or_empty(source, 1, "new")

    List.len(source) == 3
        and str_byte_count_at(source, 1, 6)
        and List.len(updated) == 3
        and str_byte_count_at(updated, 1, 3)
}

refcounted_tuple_list_set_ok : Bool
refcounted_tuple_list_set_ok = {
    source : List((Str, U32, U32))
    source = [("left", 1, 10), ("middle", 2, 20), ("right", 3, 30)]
    updated = set_sound_or_empty(source, 1, ("new", 9, 90))

    sound_at(source, 1, "middle", 2, 20)
        and sound_at(updated, 1, "new", 9, 90)
}

zero_sized_list_set_ok : Bool
zero_sized_list_set_ok = {
    source : List({})
    source = [{}, {}, {}]
    updated = set_zst_or_empty(source, 1, {})

    List.len(updated) == 3
}

main! = |seed| {
    if flat_list_ops_ok(seed) and refcounted_list_ops_ok and refcounted_list_set_ok and refcounted_tuple_list_set_ok and zero_sized_list_set_ok {
        "ok"
    } else {
        "bad"
    }
}
