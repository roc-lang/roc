# https://github.com/roc-lang/roc/issues/13281
NonZero(n) := { value : n }.{
        new : n -> Try(NonZero(n), [Zero])
                where [
                        n.is_eq : n, n -> Bool,
                        n.from_numeral : Numeral -> Try(n, [InvalidNumeral(Str)]),
                ]
        new = |value| if value == 0 Err(Zero) else Ok(NonZero.{ value })
}

main! = |_args| {
        Ok(non_zero) = NonZero.new(42)
        echo!(Str.inspect(non_zero))
        Ok({})
}
