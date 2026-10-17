# A closure built inside a function with a `where` constraint, whose body
# dispatches on its argument (`find_first_index`) and compares with the
# constrained variable (`==`). `before_colon` stores one such closure as a
# compile-time constant; `count_before` builds one at runtime. The stored one
# takes its dictionaries and descriptors from the types its stored value fixes
# (its signature, its captures, and its construction site's bindings), since
# the scope that constructed it is gone.
BoxyConstrainedClosureDictionaries(a) :: { fun : List(a) -> U64 }.{
	find_before : a -> BoxyConstrainedClosureDictionaries(a) where [a.is_eq : a, a -> Bool]
	find_before = |target| {
		fun: |input| {
			match input.find_first_index(|x| x == target) {
				Ok(index) => index
				Err(_) => 0
			}
		},
	}
}

main! = |_args| Ok({})

before_colon : BoxyConstrainedClosureDictionaries(U8)
before_colon = BoxyConstrainedClosureDictionaries.find_before(':')

count_before : U8, List(U8) -> U64
count_before = |c, bytes| {
	finder = BoxyConstrainedClosureDictionaries.find_before(c)
	(finder.fun)(bytes)
}

expect (before_colon.fun)("ab:c".to_utf8()) == 2
expect count_before(':', "abc:d".to_utf8()) == 3
