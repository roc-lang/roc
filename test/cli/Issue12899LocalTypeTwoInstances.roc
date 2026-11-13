# Repro for https://github.com/roc-lang/roc/issues/12899
# A generic function that declares a nominal type over its own type variable
# is used at two different types; both expects pass under --specialize=yes.
Issue12899LocalTypeTwoInstances :: [].{}

wrapped : a -> Str
wrapped = |x| {
	Wrap := [Wrap(a)]
	Str.inspect([Wrap.Wrap(x)])
}

expect wrapped(5.U64) == "[Wrap(5)]"
expect wrapped("hi") == "[Wrap(\"hi\")]"
