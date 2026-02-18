app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

# Minimal reproduction of is_eq method lookup failure

# Define a nominal type with a custom is_eq method
Animal := [Dog(Str), Cat(Str)].{
	is_eq = |a, b| match (a, b) {
		(Dog(name1), Dog(name2)) => name1 == name2
		(Cat(name1), Cat(name2)) => name1 == name2
		_ => Bool.False
	}
}

main! = || {
	dog : Animal
	dog = Dog("Fido")
	cat : Animal
	cat = Cat("Whiskers")

	# This line crashes with: Error evaluating: MethodLookupFailed
	result = dog == cat

	Stdout.line!(Str.inspect(result))
}
