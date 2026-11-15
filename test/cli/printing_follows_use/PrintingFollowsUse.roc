import ShownMaker

# A record a function builds or takes apart, and a tag union an exhaustive
# match closes, print as each use has them: the nominal type a use makes them,
# with its inspect override, or the plain record or tag union, under both
# lowering strategies (design.md "Methods On Open Rows").

PrintingFollowsUse :: [].{}

Shown := { id : U64 }.{
	to_inspect : Shown -> Str
	to_inspect = |s| Str.concat("shown ", s.id.to_str())

	# A method bound to an alias of a function with a row node.
	text = taken_apart
}

Light := [Red, Green].{
	to_inspect : Light -> Str
	to_inspect = |_| "a light"

	label = name
}

Bare := { id : U64 }

shown : Shown
shown = { id: 7 }

# Builds a record, prints it, and returns it.
make_shown = |id| {
	r = { id: id }
	(Str.inspect(r), r)
}

show = |x| Str.inspect(x)

# Prints through a generic helper.
via_helper = |id| {
	r = { id: id }
	(show(r), r)
}

# Prints the record inside a List.
in_list = |id| {
	r = { id: id }
	(Str.inspect([r]), r)
}

# The record reaches the signature only inside a List.
list_only = |id| {
	l = [{ id: id }]
	(Str.inspect(l), l)
}

# Takes the record apart with a closed pattern and keeps it with `as`.
taken_apart = |{ id } as r| {
	_ = id
	Str.inspect(r)
}

# The tag union an exhaustive match closes, printed directly and through the
# generic helper.
name = |c| {
	n = match c {
		Red => 1
		Green => 2
	}
	(Str.inspect(c), n)
}

name_via_helper = |c| {
	n = match c {
		Red => 1
		Green => 2
	}
	(show(c), n)
}

# The tag union an exhaustive match closes is this function's only row node,
# and its type has no type variable.
light_text = |c| {
	_ = match c {
		Red => 1
		Green => 2
	}
	Str.inspect(c)
}

pick = |b| {
	c = if b Red else Green
	(Str.inspect(c), c)
}

# The record's row node is this function's only hidden input; a capture-free
# lambda inside it is called and returned.
with_lambda = |r| {
	f = |x| x + 1
	_ = r.id
	(f(1), Str.inspect(r))
}

apply = |f, x| f(x)

# A record that never leaves its function prints as the plain record.
local_only = |id| {
	r = { id: id }
	Str.inspect(r)
}

# A compile-time constant computed through a function with only a row node.
stored = taken_apart(shown)

# Top-level aliases of functions with row nodes, and an alias of an alias.
taken_apart_alias = taken_apart

alias_of_alias = taken_apart_alias

light_text_alias = light_text

red : Light
red = Red

expect {
	a : (Str, Shown)
	a = make_shown(1)
	a.0 == "shown 1"
}

# A caller that does not make the record a nominal type prints it plain.
expect make_shown(2).0 == "{ id: 2.0 }"

# Two uses of one function, one lifting and one not.
expect {
	lifted : (Str, Shown)
	lifted = make_shown(3)
	plain = make_shown(4)
	(lifted.0, plain.0) == ("shown 3", "{ id: 4.0 }")
}

expect {
	c : (Str, Shown)
	c = via_helper(5)
	c.0 == "shown 5"
}

expect {
	d : (Str, Shown)
	d = in_list(6)
	d.0 == "[shown 6]"
}

expect {
	e : (Str, List(Shown))
	e = list_only(7)
	e.0 == "[shown 7]"
}

expect taken_apart(shown) == "shown 7"

expect taken_apart({ id: 8 }) == "{ id: 8.0 }"

# Used as a function value.
expect apply(taken_apart, shown) == "shown 7"

expect {
	g : Shown -> Str
	g = taken_apart
	g(shown)
}
	== "shown 7"

expect stored == "shown 7"

expect with_lambda(shown) == (2, "shown 7")

expect {
	light : Light
	light = Red
	name(light).0 == "a light"
}

expect name(Green).0 == "Green"

expect {
	light : Light
	light = Green
	name_via_helper(light).0 == "a light"
}

expect {
	f : (Str, Light)
	f = pick(Bool.True)
	f.0 == "a light"
}

# A nominal type with no inspect override prints its record.
expect {
	j : (Str, Bare)
	j = make_shown(9)
	j.0 == "{ id: 9 }"
}

expect local_only(10) == "{ id: 10.0 }"

# The same through a function in another module.
expect {
	m : (Str, Shown)
	m = ShownMaker.make(11)
	m.0 == "shown 11"
}

expect ShownMaker.make(12).0 == "{ id: 12.0 }"

expect {
	light : Light
	light = Red
	ShownMaker.describe(light).0 == "a light"
}

expect ShownMaker.describe(Green).0 == "Green"

# An alias of a function prints what each use of the alias supplies
# (design.md "Procedure Aliases").
expect {
	fv = taken_apart
	fv(shown) == "shown 7"
}

expect {
	a = taken_apart
	b = a
	b(shown) == "shown 7"
}

expect taken_apart_alias(shown) == "shown 7"

expect alias_of_alias(shown) == "shown 7"

expect {
	fv = taken_apart_alias
	fv(shown) == "shown 7"
}

expect ShownMaker.peek_alias(shown) == "shown 7"

expect {
	fv = ShownMaker.peek
	fv(shown) == "shown 7"
}

expect {
	fv = ShownMaker.peek_alias
	fv(shown) == "shown 7"
}

expect {
	fv = taken_apart
	apply(fv, shown) == "shown 7"
}

expect apply(taken_apart_alias, shown) == "shown 7"

expect {
	fv = taken_apart
	List.map([shown], fv) == ["shown 7"]
}

expect {
	fv = taken_apart
	r = { f: fv }
	(r.f)(shown) == "shown 7"
}

# Two uses of one alias, one lifting and one not.
expect {
	fv = taken_apart
	(fv(shown), fv({ id: 8 })) == ("shown 7", "{ id: 8.0 }")
}

expect {
	light : Light
	light = Red
	fv = name
	(fv(light).0, fv(Green).0) == ("a light", "Green")
}

expect {
	fv = light_text
	(fv(red), fv(Green)) == ("a light", "Green")
}

expect {
	a = light_text
	b = a
	b(red) == "a light"
}

expect {
	fv = light_text
	List.map([red], fv) == ["a light"]
}

expect light_text_alias(red) == "a light"

expect {
	fv = light_text_alias
	(fv(red), fv(Green)) == ("a light", "Green")
}

# Methods bound to aliases.
expect shown.text() == "shown 7"

expect {
	light : Light
	light = Green
	light.label().0 == "a light"
}
