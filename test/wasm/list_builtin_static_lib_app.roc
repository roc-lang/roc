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

flat_list_ops_ok : Bool
flat_list_ops_ok = {
    appended = List.append([1.U8, 2.U8, 3.U8], 4.U8)
    concatted = List.concat(appended, [5.U8, 6.U8])
    dropped = List.drop_at(concatted, 2)
    reversed = List.rev(dropped)

    List.len(reversed) == 5
        and u8_at(reversed, 0, 6.U8)
        and u8_at(reversed, 1, 5.U8)
        and u8_at(reversed, 2, 4.U8)
        and u8_at(reversed, 3, 2.U8)
        and u8_at(reversed, 4, 1.U8)
}

refcounted_list_ops_ok : Bool
refcounted_list_ops_ok = {
    concatted = List.concat(["a", "bb"], ["ccc"])
    dropped = List.drop_at(concatted, 1)

    List.len(dropped) == 2
        and str_byte_count_at(dropped, 0, 1)
        and str_byte_count_at(dropped, 1, 3)
}

main! = || {
    if flat_list_ops_ok and refcounted_list_ops_ok {
        "ok"
    } else {
        "bad"
    }
}
