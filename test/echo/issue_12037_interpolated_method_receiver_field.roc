# Interpolation needs every interpolated value to have one type, and `age` is
# a `U64` beside the `Str` name, so `describe` cannot take a `Person`. The
# program runs until it calls `describe`.
Person := { name : Str, age : U64 }.{
	describe = |person| "${person.name} is ${person.age} years old"
}

main! = |_args| {
	echo!("before")
	ada = Person.{ name: "Ada", age: 36 }
	echo!(ada.describe())
	Ok({})
}
