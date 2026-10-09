BoxySharedCallOperand := {}

# One value passed in two argument positions of the same call. The callee
# takes the function out of its first argument and reads its second
# afterwards, so the caller must not give up its only reference through the
# first position.

capture = |value| |_| value

Word := { get : {} -> Str }.{
    make : Str -> Word
    make = |text| { get: capture(text) }

    same : Word, Word -> Bool
    same = |x, y| (x.get)({}) == (y.get)({})
}

expect {
    w = Word.make(Str.concat("a string long enough to live on the heap", "!"))
    Word.same(w, w)
}

expect {
    w = Word.make("short")
    Word.same(w, w)
}
