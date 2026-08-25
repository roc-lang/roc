app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdin
import pf.Stdout

shift_dec : U64, Dec -> U8
shift_dec = |value, count| value.shr_wrap(count.to_u8_wrap()).to_u8_wrap()

shift_i64 : U64, I64 -> U8
shift_i64 = |value, count| value.shr_wrap(count.to_u8_wrap()).to_u8_wrap()

convert_dec : Dec -> U8
convert_dec = |count| count.to_u8_wrap()

main! = || {
    value = match U64.from_str(Stdin.line!()) {
        Ok(number) => number
        Err(_) => 0
    }
    Stdout.line!(Str.inspect([convert_dec(56), shift_dec(value, 56), shift_i64(value, 56)]))
}
