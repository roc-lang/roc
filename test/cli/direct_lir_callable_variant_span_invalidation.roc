app [main!] { pf: platform "../fx/platform/main.roc" }

import pf.Stdout

apply_one : (Str -> Str) -> Str
apply_one = |f| f("one")

apply_two : (Str -> Str) -> Str
apply_two = |f| f("two")

choose_apply : Bool -> ((Str -> Str) -> Str)
choose_apply = |flag| {
    if flag {
        apply_one
    } else {
        apply_two
    }
}

decorate_left : Str -> Str
decorate_left = |value| Str.concat("<", value)

decorate_right : Str -> Str
decorate_right = |value| Str.concat(value, ">")

decorate_00 : Str -> Str
decorate_00 = |value| Str.concat("00", value)

decorate_01 : Str -> Str
decorate_01 = |value| Str.concat("01", value)

decorate_02 : Str -> Str
decorate_02 = |value| Str.concat("02", value)

decorate_03 : Str -> Str
decorate_03 = |value| Str.concat("03", value)

decorate_04 : Str -> Str
decorate_04 = |value| Str.concat("04", value)

decorate_05 : Str -> Str
decorate_05 = |value| Str.concat("05", value)

decorate_06 : Str -> Str
decorate_06 = |value| Str.concat("06", value)

decorate_07 : Str -> Str
decorate_07 = |value| Str.concat("07", value)

decorate_08 : Str -> Str
decorate_08 = |value| Str.concat("08", value)

decorate_09 : Str -> Str
decorate_09 = |value| Str.concat("09", value)

decorate_10 : Str -> Str
decorate_10 = |value| Str.concat("10", value)

decorate_11 : Str -> Str
decorate_11 = |value| Str.concat("11", value)

decorate_12 : Str -> Str
decorate_12 = |value| Str.concat("12", value)

decorate_13 : Str -> Str
decorate_13 = |value| Str.concat("13", value)

decorate_14 : Str -> Str
decorate_14 = |value| Str.concat("14", value)

decorate_15 : Str -> Str
decorate_15 = |value| Str.concat("15", value)

Choice := [
    UseLeft,
    UseRight,
    Use00,
    Use01,
    Use02,
    Use03,
    Use04,
    Use05,
    Use06,
    Use07,
    Use08,
    Use09,
    Use10,
    Use11,
    Use12,
    Use13,
    Use14,
    Use15,
]

choose_decorate : Choice -> (Str -> Str)
choose_decorate = |choice| {
    match choice {
        UseLeft => decorate_left
        UseRight => decorate_right
        Use00 => decorate_00
        Use01 => decorate_01
        Use02 => decorate_02
        Use03 => decorate_03
        Use04 => decorate_04
        Use05 => decorate_05
        Use06 => decorate_06
        Use07 => decorate_07
        Use08 => decorate_08
        Use09 => decorate_09
        Use10 => decorate_10
        Use11 => decorate_11
        Use12 => decorate_12
        Use13 => decorate_13
        Use14 => decorate_14
        Use15 => decorate_15
    }
}

main! = || {
    selected = choose_apply(True)
    rendered = selected(choose_decorate(Use15))
    Stdout.line!(rendered)
}
