app [run!] { pf: platform "../../alloc-count/platform/main.roc", pk: "pk/main.roc" }

import pk.Pdf

run! : Str => Str
run! = |input| {
	mode = if check(input.count_utf8_bytes()) Pdf.Mode.On else Pdf.Mode.Off
	options = Pdf.Options.{ mode }
	again = Pdf.Options.{ mode }
	if options.theme.body.size != 11 crash "wrong nested default"
	if options.theme.bullet_indent.raw() != again.theme.bullet_indent.raw() crash "inconsistent numeric default"
	options.theme.bullet_indent.raw().to_str()
}

check : U64 -> Bool
check = |n| if n > 50 crash "too many" else n > 1
