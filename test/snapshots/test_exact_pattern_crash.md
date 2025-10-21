# META
~~~ini
description=Exact pattern from type_alias_parameterized with variations
type=file
~~~
# SOURCE
~~~roc
app [main] { pf: platform "../basic-cli/platform.roc" }

# Type alias with parameters, just like the original
Pair(a, b) : (a, b)

# Function that uses the alias and will need instantiation
swap_pair : Pair(a, b) -> Pair(b, a)
swap_pair = |(x, y)| (y, x)

# Another polymorphic function to create more complex instantiation
map_pair : Pair(a, b), (a -> c), (b -> d) -> Pair(c, d)
map_pair = |(x, y), f, g| (f(x), g(y))

# This should trigger multiple instantiations
# First swap_pair gets instantiated, then map_pair
# The error should involve deeply nested instantiated types
main = {
    # This creates Pair(Num, Num)
    p1 = swap_pair((1, 2))

    # This should fail - map_pair expects a tuple but gets four separate arguments
    # And the instantiated types from map_pair should cause issues
    p2 = map_pair(3, 4, (|x| x + 1), (|y| y * 2))

    p2
}
~~~
# EXPECTED
UNUSED VARIABLE - test_exact_pattern_crash.md:19:5:19:7
TYPE MISMATCH - test_exact_pattern_crash.md:23:10:23:50
# PROBLEMS
**UNUSED VARIABLE**
Variable `p1` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_p1` to suppress this warning.
The unused variable is declared here:
**test_exact_pattern_crash.md:19:5:19:7:**
```roc
    p1 = swap_pair((1, 2))
```
    ^^


**TYPE MISMATCH**
This expression is used in an unexpected way:
**test_exact_pattern_crash.md:23:10:23:50:**
```roc
    p2 = map_pair(3, 4, (|x| x + 1), (|y| y * 2))
```
         ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

It has the type:
    _Num(_size), Num(_size2), Num(_size3) -> Num(_size4), Num(_size5) -> Num(_size6) -> _ret_

But I expected it to be:
    _Pair(a, b), a -> c, b -> d -> Pair(c, d)_

# TOKENS
~~~zig
KwApp,OpenSquare,LowerIdent,CloseSquare,OpenCurly,LowerIdent,OpColon,KwPlatform,StringStart,StringPart,StringEnd,CloseCurly,
UpperIdent,NoSpaceOpenRound,LowerIdent,Comma,LowerIdent,CloseRound,OpColon,OpenRound,LowerIdent,Comma,LowerIdent,CloseRound,
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,LowerIdent,Comma,LowerIdent,CloseRound,OpArrow,UpperIdent,NoSpaceOpenRound,LowerIdent,Comma,LowerIdent,CloseRound,
LowerIdent,OpAssign,OpBar,NoSpaceOpenRound,LowerIdent,Comma,LowerIdent,CloseRound,OpBar,OpenRound,LowerIdent,Comma,LowerIdent,CloseRound,
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,LowerIdent,Comma,LowerIdent,CloseRound,Comma,OpenRound,LowerIdent,OpArrow,LowerIdent,CloseRound,Comma,OpenRound,LowerIdent,OpArrow,LowerIdent,CloseRound,OpArrow,UpperIdent,NoSpaceOpenRound,LowerIdent,Comma,LowerIdent,CloseRound,
LowerIdent,OpAssign,OpBar,NoSpaceOpenRound,LowerIdent,Comma,LowerIdent,CloseRound,Comma,LowerIdent,Comma,LowerIdent,OpBar,OpenRound,LowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,Comma,LowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,CloseRound,
LowerIdent,OpAssign,OpenCurly,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,NoSpaceOpenRound,Int,Comma,Int,CloseRound,CloseRound,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,Int,Comma,Int,Comma,OpenRound,OpBar,LowerIdent,OpBar,LowerIdent,OpPlus,Int,CloseRound,Comma,OpenRound,OpBar,LowerIdent,OpBar,LowerIdent,OpStar,Int,CloseRound,CloseRound,
LowerIdent,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(app
		(provides
			(exposed-lower-ident
				(text "main")))
		(record-field (name "pf")
			(e-string
				(e-string-part (raw "../basic-cli/platform.roc"))))
		(packages
			(record-field (name "pf")
				(e-string
					(e-string-part (raw "../basic-cli/platform.roc"))))))
	(statements
		(s-type-decl
			(header (name "Pair")
				(args
					(ty-var (raw "a"))
					(ty-var (raw "b"))))
			(ty-tuple
				(ty-var (raw "a"))
				(ty-var (raw "b"))))
		(s-type-anno (name "swap_pair")
			(ty-fn
				(ty-apply
					(ty (name "Pair"))
					(ty-var (raw "a"))
					(ty-var (raw "b")))
				(ty-apply
					(ty (name "Pair"))
					(ty-var (raw "b"))
					(ty-var (raw "a")))))
		(s-decl
			(p-ident (raw "swap_pair"))
			(e-lambda
				(args
					(p-tuple
						(p-ident (raw "x"))
						(p-ident (raw "y"))))
				(e-tuple
					(e-ident (raw "y"))
					(e-ident (raw "x")))))
		(s-type-anno (name "map_pair")
			(ty-fn
				(ty-apply
					(ty (name "Pair"))
					(ty-var (raw "a"))
					(ty-var (raw "b")))
				(ty-fn
					(ty-var (raw "a"))
					(ty-var (raw "c")))
				(ty-fn
					(ty-var (raw "b"))
					(ty-var (raw "d")))
				(ty-apply
					(ty (name "Pair"))
					(ty-var (raw "c"))
					(ty-var (raw "d")))))
		(s-decl
			(p-ident (raw "map_pair"))
			(e-lambda
				(args
					(p-tuple
						(p-ident (raw "x"))
						(p-ident (raw "y")))
					(p-ident (raw "f"))
					(p-ident (raw "g")))
				(e-tuple
					(e-apply
						(e-ident (raw "f"))
						(e-ident (raw "x")))
					(e-apply
						(e-ident (raw "g"))
						(e-ident (raw "y"))))))
		(s-decl
			(p-ident (raw "main"))
			(e-block
				(statements
					(s-decl
						(p-ident (raw "p1"))
						(e-apply
							(e-ident (raw "swap_pair"))
							(e-tuple
								(e-int (raw "1"))
								(e-int (raw "2")))))
					(s-decl
						(p-ident (raw "p2"))
						(e-apply
							(e-ident (raw "map_pair"))
							(e-int (raw "3"))
							(e-int (raw "4"))
							(e-tuple
								(e-lambda
									(args
										(p-ident (raw "x")))
									(e-binop (op "+")
										(e-ident (raw "x"))
										(e-int (raw "1")))))
							(e-tuple
								(e-lambda
									(args
										(p-ident (raw "y")))
									(e-binop (op "*")
										(e-ident (raw "y"))
										(e-int (raw "2")))))))
					(e-ident (raw "p2")))))))
~~~
# FORMATTED
~~~roc
app [main] { pf: platform "../basic-cli/platform.roc" }

# Type alias with parameters, just like the original
Pair(a, b) : (a, b)

# Function that uses the alias and will need instantiation
swap_pair : Pair(a, b) -> Pair(b, a)
swap_pair = |(x, y)| (y, x)

# Another polymorphic function to create more complex instantiation
map_pair : Pair(a, b), (a -> c), (b -> d) -> Pair(c, d)
map_pair = |(x, y), f, g| (f(x), g(y))

# This should trigger multiple instantiations
# First swap_pair gets instantiated, then map_pair
# The error should involve deeply nested instantiated types
main = {
	# This creates Pair(Num, Num)
	p1 = swap_pair((1, 2))

	# This should fail - map_pair expects a tuple but gets four separate arguments
	# And the instantiated types from map_pair should cause issues
	p2 = map_pair(3, 4, (|x| x + 1), (|y| y * 2))

	p2
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "swap_pair"))
		(e-lambda
			(args
				(p-tuple
					(patterns
						(p-assign (ident "x"))
						(p-assign (ident "y")))))
			(e-tuple
				(elems
					(e-lookup-local
						(p-assign (ident "y")))
					(e-lookup-local
						(p-assign (ident "x"))))))
		(annotation
			(ty-fn (effectful false)
				(ty-apply (name "Pair") (local)
					(ty-rigid-var (name "a"))
					(ty-rigid-var (name "b")))
				(ty-apply (name "Pair") (local)
					(ty-rigid-var-lookup (ty-rigid-var (name "b")))
					(ty-rigid-var-lookup (ty-rigid-var (name "a")))))))
	(d-let
		(p-assign (ident "map_pair"))
		(e-lambda
			(args
				(p-tuple
					(patterns
						(p-assign (ident "x"))
						(p-assign (ident "y"))))
				(p-assign (ident "f"))
				(p-assign (ident "g")))
			(e-tuple
				(elems
					(e-call
						(e-lookup-local
							(p-assign (ident "f")))
						(e-lookup-local
							(p-assign (ident "x"))))
					(e-call
						(e-lookup-local
							(p-assign (ident "g")))
						(e-lookup-local
							(p-assign (ident "y")))))))
		(annotation
			(ty-fn (effectful false)
				(ty-apply (name "Pair") (local)
					(ty-rigid-var (name "a"))
					(ty-rigid-var (name "b")))
				(ty-parens
					(ty-fn (effectful false)
						(ty-rigid-var-lookup (ty-rigid-var (name "a")))
						(ty-rigid-var (name "c"))))
				(ty-parens
					(ty-fn (effectful false)
						(ty-rigid-var-lookup (ty-rigid-var (name "b")))
						(ty-rigid-var (name "d"))))
				(ty-apply (name "Pair") (local)
					(ty-rigid-var-lookup (ty-rigid-var (name "c")))
					(ty-rigid-var-lookup (ty-rigid-var (name "d")))))))
	(d-let
		(p-assign (ident "main"))
		(e-block
			(s-let
				(p-assign (ident "p1"))
				(e-call
					(e-lookup-local
						(p-assign (ident "swap_pair")))
					(e-tuple
						(elems
							(e-num (value "1"))
							(e-num (value "2"))))))
			(s-let
				(p-assign (ident "p2"))
				(e-call
					(e-lookup-local
						(p-assign (ident "map_pair")))
					(e-num (value "3"))
					(e-num (value "4"))
					(e-lambda
						(args
							(p-assign (ident "x")))
						(e-binop (op "add")
							(e-lookup-local
								(p-assign (ident "x")))
							(e-num (value "1"))))
					(e-lambda
						(args
							(p-assign (ident "y")))
						(e-binop (op "mul")
							(e-lookup-local
								(p-assign (ident "y")))
							(e-num (value "2"))))))
			(e-lookup-local
				(p-assign (ident "p2")))))
	(s-alias-decl
		(ty-header (name "Pair")
			(ty-args
				(ty-rigid-var (name "a"))
				(ty-rigid-var (name "b"))))
		(ty-tuple
			(ty-rigid-var-lookup (ty-rigid-var (name "a")))
			(ty-rigid-var-lookup (ty-rigid-var (name "b"))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Pair(a, b) -> Pair(b, a)"))
		(patt (type "Pair(a, b), a -> c, b -> d -> Pair(c, d)"))
		(patt (type "_e")))
	(type_decls
		(alias (type "Pair(a, b)")
			(ty-header (name "Pair")
				(ty-args
					(ty-rigid-var (name "a"))
					(ty-rigid-var (name "b"))))))
	(expressions
		(expr (type "Pair(a, b) -> Pair(b, a)"))
		(expr (type "Pair(a, b), a -> c, b -> d -> Pair(c, d)"))
		(expr (type "_e"))))
~~~
