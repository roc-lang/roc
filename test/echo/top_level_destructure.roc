# Names bound by a top-level destructure are ordinary top-level values: a
# destructured function is callable (including one that captures a block-local
# binding of the destructured value, or reads a sibling name bound by the same
# destructure), a destructured function over a literal is as polymorphic as a
# plainly defined one, a destructured value is readable, a tuple destructure
# works the same way, and any of them may be referenced ahead of its
# declaration like a plainly named top-level value.
first = greet("Ada")

{ greet, punctuation } = { greet: |name| Str.concat("Hello, ", name), punctuation: "!" }
(double, base) = (|n| n * 2, 21)
{ shout } = {
	suffix = "!!"
	{ shout: |s| Str.concat(s, suffix) }
}
{ wave, greeting } = { wave: |name| Str.concat(greeting, name), greeting: "o/ " }
{ same } = { same: |x| x }

main! = |_args| {
	echo!("${first}${punctuation}\n")
	echo!("${greet("Grace")}${punctuation}\n")
	echo!("${double(base).to_str()}\n")
	echo!("${shout("hey")}\n")
	echo!("${wave("Linus")}\n")
	echo!("${same("same")} ${same(3).to_str()}\n")
	Ok({})
}
