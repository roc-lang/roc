# META
~~~ini
description=Test showing type error that would occur if rigid variables were not instantiated
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/platform.roc" }

# Polymorphic function that swaps elements of a tuple
swap : (a, b) -> (b, a)
swap = |pair| {
    (x, y) = pair
    (y, x)
}

# Multiple uses that would conflict if 'a' and 'b' weren't instantiated
main! = |_| {
    # First use: swap (Int, Str)
    result1 = swap((42, "hello"))
    
    # Second use: swap (Bool, List Int)
    # This would fail if 'a' and 'b' from the first call were reused
    result2 = swap((Bool.true, [1, 2, 3]))
    
    # Third use: swap (Str, Str) 
    # This shows even when both types are the same, we still need fresh vars
    result3 = swap(("foo", "bar"))
    
    {}
}
~~~
# EXPECTED
UNUSED VARIABLE - rigid_var_no_instantiation_error.md:13:5:13:12
UNUSED VARIABLE - rigid_var_no_instantiation_error.md:17:5:17:12
UNUSED VARIABLE - rigid_var_no_instantiation_error.md:21:5:21:12
# PROBLEMS
**UNUSED VARIABLE**
Variable `result1` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_result1` to suppress this warning.
The unused variable is declared here:
**rigid_var_no_instantiation_error.md:13:5:13:12:**
```roc
    result1 = swap((42, "hello"))
```
    ^^^^^^^


**UNUSED VARIABLE**
Variable `result2` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_result2` to suppress this warning.
The unused variable is declared here:
**rigid_var_no_instantiation_error.md:17:5:17:12:**
```roc
    result2 = swap((Bool.true, [1, 2, 3]))
```
    ^^^^^^^


**UNUSED VARIABLE**
Variable `result3` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_result3` to suppress this warning.
The unused variable is declared here:
**rigid_var_no_instantiation_error.md:21:5:21:12:**
```roc
    result3 = swap(("foo", "bar"))
```
    ^^^^^^^


# TOKENS
~~~zig
KwApp,OpenSquare,LowerIdent,CloseSquare,OpenCurly,LowerIdent,OpColon,KwPlatform,StringStart,StringPart,StringEnd,CloseCurly,
LowerIdent,OpColon,OpenRound,LowerIdent,Comma,LowerIdent,CloseRound,OpArrow,OpenRound,LowerIdent,Comma,LowerIdent,CloseRound,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenCurly,
OpenRound,LowerIdent,Comma,LowerIdent,CloseRound,OpAssign,LowerIdent,
OpenRound,LowerIdent,Comma,LowerIdent,CloseRound,
CloseCurly,
LowerIdent,OpAssign,OpBar,Underscore,OpBar,OpenCurly,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,NoSpaceOpenRound,Int,Comma,StringStart,StringPart,StringEnd,CloseRound,CloseRound,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,NoSpaceOpenRound,UpperIdent,NoSpaceDotLowerIdent,Comma,OpenSquare,Int,Comma,Int,Comma,Int,CloseSquare,CloseRound,CloseRound,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,NoSpaceOpenRound,StringStart,StringPart,StringEnd,Comma,StringStart,StringPart,StringEnd,CloseRound,CloseRound,
OpenCurly,CloseCurly,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(app
		(provides
			(exposed-lower-ident
				(text "main!")))
		(record-field (name "pf")
			(e-string
				(e-string-part (raw "../basic-cli/platform.roc"))))
		(packages
			(record-field (name "pf")
				(e-string
					(e-string-part (raw "../basic-cli/platform.roc"))))))
	(statements
		(s-type-anno (name "swap")
			(ty-fn
				(ty-tuple
					(ty-var (raw "a"))
					(ty-var (raw "b")))
				(ty-tuple
					(ty-var (raw "b"))
					(ty-var (raw "a")))))
		(s-decl
			(p-ident (raw "swap"))
			(e-lambda
				(args
					(p-ident (raw "pair")))
				(e-block
					(statements
						(s-decl
							(p-tuple
								(p-ident (raw "x"))
								(p-ident (raw "y")))
							(e-ident (raw "pair")))
						(e-tuple
							(e-ident (raw "y"))
							(e-ident (raw "x")))))))
		(s-decl
			(p-ident (raw "main!"))
			(e-lambda
				(args
					(p-underscore))
				(e-block
					(statements
						(s-decl
							(p-ident (raw "result1"))
							(e-apply
								(e-ident (raw "swap"))
								(e-tuple
									(e-int (raw "42"))
									(e-string
										(e-string-part (raw "hello"))))))
						(s-decl
							(p-ident (raw "result2"))
							(e-apply
								(e-ident (raw "swap"))
								(e-tuple
									(e-ident (raw "Bool.true"))
									(e-list
										(e-int (raw "1"))
										(e-int (raw "2"))
										(e-int (raw "3"))))))
						(s-decl
							(p-ident (raw "result3"))
							(e-apply
								(e-ident (raw "swap"))
								(e-tuple
									(e-string
										(e-string-part (raw "foo")))
									(e-string
										(e-string-part (raw "bar"))))))
						(e-record)))))))
~~~
# FORMATTED
~~~roc
app [main!] { pf: platform "../basic-cli/platform.roc" }

# Polymorphic function that swaps elements of a tuple
swap : (a, b) -> (b, a)
swap = |pair| {
	(x, y) = pair
	(y, x)
}

# Multiple uses that would conflict if 'a' and 'b' weren't instantiated
main! = |_| {
	# First use: swap (Int, Str)
	result1 = swap((42, "hello"))

	# Second use: swap (Bool, List Int)
	# This would fail if 'a' and 'b' from the first call were reused
	result2 = swap((Bool.true, [1, 2, 3]))

	# Third use: swap (Str, Str) 
	# This shows even when both types are the same, we still need fresh vars
	result3 = swap(("foo", "bar"))

	{}
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "swap"))
		(e-lambda
			(args
				(p-assign (ident "pair")))
			(e-block
				(s-let
					(p-tuple
						(patterns
							(p-assign (ident "x"))
							(p-assign (ident "y"))))
					(e-lookup-local
						(p-assign (ident "pair"))))
				(e-tuple
					(elems
						(e-lookup-local
							(p-assign (ident "y")))
						(e-lookup-local
							(p-assign (ident "x")))))))
		(annotation
			(declared-type
				(ty-fn (effectful false)
					(ty-tuple
						(ty-rigid-var (name "a"))
						(ty-rigid-var (name "b")))
					(ty-tuple
						(ty-rigid-var-lookup (ty-rigid-var (name "b")))
						(ty-rigid-var-lookup (ty-rigid-var (name "a"))))))))
	(d-let
		(p-assign (ident "main!"))
		(e-closure
			(captures
				(capture (ident "swap")))
			(e-lambda
				(args
					(p-underscore))
				(e-block
					(s-let
						(p-assign (ident "result1"))
						(e-call
							(e-lookup-local
								(p-assign (ident "swap")))
							(e-tuple
								(elems
									(e-num (value "42"))
									(e-string
										(e-literal (string "hello")))))))
					(s-let
						(p-assign (ident "result2"))
						(e-call
							(e-lookup-local
								(p-assign (ident "swap")))
							(e-tuple
								(elems
									(e-lookup-external
										(module-idx "2")
										(target-node-idx "0"))
									(e-list
										(elems
											(e-num (value "1"))
											(e-num (value "2"))
											(e-num (value "3"))))))))
					(s-let
						(p-assign (ident "result3"))
						(e-call
							(e-lookup-local
								(p-assign (ident "swap")))
							(e-tuple
								(elems
									(e-string
										(e-literal (string "foo")))
									(e-string
										(e-literal (string "bar")))))))
					(e-empty_record))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "(a, b) -> (b, a)"))
		(patt (type "_arg -> {}")))
	(expressions
		(expr (type "(a, b) -> (b, a)"))
		(expr (type "_arg -> {}"))))
~~~
