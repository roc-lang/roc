BoxyStoredGenericClosure := {}

# repro for https://github.com/roc-lang/roc/issues/11873: a compile-time
# constant holds a closure from a generic function. Boxy describes the
# closure's type variables from the stored value, not from the frame that
# restores it.
map = |mapper| |_state| mapper(0)

u8 = map(|_| 0)

Format := [Default].{
	decode_u8 : Format, {} -> (Try(U8, Str), {})
	decode_u8 = |_fmt, state| (Ok(u8(state)), state)
}

val : {} -> Try(a, Str) where [a.decode : {}, Format -> (Try(a, Str), {})]
val = |rs| {
	Shape : a
	Shape.decode(rs, Format.Default).0
}

check : Try(U8, Str) -> Bool
check = |_| True

expect check(val({}))

label : {} -> Str
label = map(|n| "n=${n.to_str()}")

expect label({}) == "n=0.0"
