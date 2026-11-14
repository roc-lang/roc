# Regression for https://github.com/roc-lang/roc/issues/11726
#
# `use` and `shout` never fix `Sql`'s parameter, and nothing in `helper`
# relates its literals to anything else, so each literal's type is `Sql(v)`
# for a `v` that no scheme quantifies and nothing constrains. Such a variable
# is decided: each literal is converted once, at compile time, under either
# strategy, and `helper` reads the converted values.
Sql(a) := { text : Str }.{
	from_quote : Str -> Try(Sql(a), [BadQuotedBytes(Str)])
	from_quote = |raw|
		if raw == "bad" {
			Err(BadQuotedBytes("rejected"))
		} else {
			Ok(Sql.{ text: raw.repeat(2) })
		}

	shout : Sql(a) -> Str
	shout = |sql| "${sql.text}!"
}

use : Sql(a) -> {}
use = |_| {}

helper : Str -> Str
helper = |_which| {
	use("good")
	Sql.shout("fine")
}

# A literal whose whole type is undecided is a `Str`, and stays one.
dormant : {} -> {}
dormant = |_| {
	_ = "bad"
	{}
}

main! = |args| {
	echo!("${helper(args.len().to_str())}\n")
	_ = "bad"
	echo!("ran\n")
	Ok({})
}
