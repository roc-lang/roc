# repro for https://github.com/roc-lang/roc/issues/9892
# An associated method defined by reference to a top-level function
# (decode_u8 = generate_u8) must register the referenced procedure as the
# method target, exactly like defining the method as a direct lambda.
generate_u8 : RandomDecoder, List(U8) -> (Try(U8, [TooShort]), List(U8))
generate_u8 = |_, bytes| (Err(TooShort), bytes)

RandomDecoder := {}.{ decode_u8 = generate_u8 }
random_decoder = RandomDecoder.({})

run : _, _ -> (Try(a, _), _) where [a.decode : _, _ -> (Try(a, _), _)]
run = |bytes, decoder| {
    Val : a
    Val.decode(bytes, decoder)
}

Single(a) := a.{
    decode: _, _ -> (Try(Single(a), _), _) where [a.decode : _, _ -> (Try(a, _), _)]
    decode = |bytes, format| {
        ValA : a
        (res_a, bytes_a) = ValA.decode(bytes, format)
        match res_a {
            Ok(val_a) => (Ok(Single.(val_a)), bytes_a)
            Err(e) => (Err(e), bytes_a)
        }
    }
}

main! = |_| {
    x : (Try(Single(U8), _), List(U8))
    x = run([1, 2], random_decoder)
    _ = x
    Ok({})
}
