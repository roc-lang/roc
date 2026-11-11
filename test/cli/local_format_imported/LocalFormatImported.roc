LocalFormatImported :: [].{}

import LocalFormatImportedLib

stored_encoder = LocalFormatImportedLib.make_encoder(5.U64)

expect {
	name : Str
	name = "abc"
	stored_encoder({ count: 7, name }, 0) == Ok(122)
}

expect {
	name : Str
	name = "abc"
	LocalFormatImportedLib.make_encoder("x")({ count: 7, name }, 0) == Ok(122)
}

expect LocalFormatImportedLib.local_format(5.U64) == Ok(22)

expect LocalFormatImportedLib.local_format("hi") == Ok(22)
