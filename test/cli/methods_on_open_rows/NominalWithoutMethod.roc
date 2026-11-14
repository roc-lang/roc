import OpenRowMethods

# A nominal type without the method a function calls on its open record is
# rejected at the call, through `==` and through `foo`, in this module and
# through an imported function.

NominalWithoutMethod :: [].{}

Plain := { id : U64 }

p1 : Plain
p1 = { id: 1 }

p2 : Plain
p2 = { id: 2 }

same = |l, r| {
	_ = l.id + r.id
	l == r
}

read_foo = |l, r| {
	_ = l.id + r.id
	l.foo(r)
}

expect same(p1, p2)
expect read_foo(p1, p2)
expect OpenRowMethods.same(p1, p2)
expect OpenRowMethods.read_foo(p1, p2)
