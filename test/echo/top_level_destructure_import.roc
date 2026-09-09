# An importer uses names another module binds through top-level destructures:
# a destructured function and destructured values, referenced qualified,
# unqualified, and from inside a lambda.
import DestructuredExports exposing [greet, double]

later = |name| greet(name)

main! = |_args| {
	echo!("${greet("Ada")}\n")
	echo!("${double(DestructuredExports.base).to_str()}\n")
	echo!("${later("Bo")}\n")
	Ok({})
}
