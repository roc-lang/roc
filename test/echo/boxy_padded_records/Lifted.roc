## Values of record rows this module never relates to a nominal. Each use in
## another module checks at its own copy of the type, so a use can lift the row
## into a padded nominal this module does not name.
Lifted := {}.{
	plain = { z: 7.U32, a: 8.U64 }
	plains = [{ z: 7.U32, a: 8.U64 }, { z: 9.U32, a: 10.U64 }]
	to_plain = |z| { z, a: 3.U64 }
	nothing = {}
	beside = { k: {}, n: 5.U64 }
}
