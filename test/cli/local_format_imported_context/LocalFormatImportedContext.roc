LocalFormatImportedContext :: [].{}

import LocalFormatImportedContextLib

Apples := [Apples(U64)].{
	sample : () -> Apples
	sample = || Apples.Apples(3)
}

Pears := [Pears(Str, Str)].{
	sample : () -> Pears
	sample = || Pears.Pears("green", "pear")
}

encode : ({ count : U64, name : Str }, U64 -> Try(U64, [])) -> Try(U64, [])
encode = |encoder| {
	name : Str
	name = "abc"
	encoder({ count: 7, name }, 0)
}

# Each `encode_str` adds the byte length of `Str.inspect` of values made through
# `a.sample`, so the two instantiations compute different results.
expect encode(LocalFormatImportedContextLib.make_promoted(Apples.sample())) == Ok(44)
expect encode(LocalFormatImportedContextLib.make_promoted(Pears.sample())) == Ok(70)

expect LocalFormatImportedContextLib.make_local(5.U64)({ name: "x" }, { name: "ab" }) == Ok({ name: "ab" })

# Encoding "recurse" encodes the state, whose name is "ab".
expect LocalFormatImportedContextLib.make_local("five")({ name: "recurse" }, { name: "ab" }) == Err(Nested)

expect LocalFormatImportedContextLib.make_local(5.U64)({ name: "recurse" }, { name: "cd" }) == Ok({ name: "cd" })
