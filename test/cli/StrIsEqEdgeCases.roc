StrIsEqEdgeCases :: [].{}

expect {
	"abcdefgh" == "abcdefgh"
}

expect {
	"abcdefghi" != "abcdefghh"
}

expect {
	"abcdefghijklmnop" != "abcdefghijklmnoX"
}

expect {
	"abcdefghijklmnopqrstuvw" != "abcdefghijklmnopqrstuvX"
}

expect {
	"abcdefghijklmnopqrstuvwx" == "abcdefghijklmnopqrstuvwx"
}

expect {
	"caf\u(00E9)" == "caf\u(00E9)"
}

expect {
	"caf\u(00E9)" != "cafe\u(0301)"
}
