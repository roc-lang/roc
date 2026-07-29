DivergentLiteralConversion :: [].{}

Wrap := { n : U64 }.{
	describe : Wrap -> Str
	describe = |_| "wrap"
}

Plain := {}.{}

## A where-clause helper used at a type that does not satisfy it. The failing
## call is reported, and the literal beside it must not be lowered as though a
## string literal could diverge.
label : a -> Str where [a.describe : a -> Str]
label = |x| x.describe()

main : Bool
main = Str.is_eq(label(Plain.{}), "nope")
