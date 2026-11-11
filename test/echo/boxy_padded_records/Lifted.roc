## Functions returning records this module never relates to a nominal. Each
## call in another module copies the result's type, so a call can lift the
## record into a padded nominal this module does not name.
Lifted := {}.{
	plain = |_| { z: 7.U32, a: 8.U64 }
	plains = |_| [{ z: 7.U32, a: 8.U64 }, { z: 9.U32, a: 10.U64 }]
	to_plain = |z| { z, a: 3.U64 }
	nothing = |_| {}
	beside = |_| { k: {}, n: 5.U64 }
}
