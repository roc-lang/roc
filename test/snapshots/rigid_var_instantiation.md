# META
~~~ini
description=Polymorphic function with rigid type variable used at multiple call sites
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/platform.roc" }

# Polymorphic identity function with rigid type variable 'a'
identity : a -> a
identity = |x| x

# Use identity at different call sites with different types
main! = |_| {
    # First call with number
    num = identity(42)
    
    # Second call with string
    str = identity("hello")
    
    # Third call with list
    lst = identity([1, 2, 3])
    
    {}
}
~~~
# EXPECTED
UNUSED VARIABLE - rigid_var_instantiation.md:10:5:10:8
UNUSED VARIABLE - rigid_var_instantiation.md:13:5:13:8
UNUSED VARIABLE - rigid_var_instantiation.md:16:5:16:8
# PROBLEMS
**UNUSED VARIABLE**
Variable `num` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_num` to suppress this warning.
The unused variable is declared here:
**rigid_var_instantiation.md:10:5:10:8:**
```roc
    num = identity(42)
```
    ^^^


**UNUSED VARIABLE**
Variable `str` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_str` to suppress this warning.
The unused variable is declared here:
**rigid_var_instantiation.md:13:5:13:8:**
```roc
    str = identity("hello")
```
    ^^^


**UNUSED VARIABLE**
Variable `lst` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_lst` to suppress this warning.
The unused variable is declared here:
**rigid_var_instantiation.md:16:5:16:8:**
```roc
    lst = identity([1, 2, 3])
```
    ^^^


# TOKENS
~~~zig
KwApp,OpenSquare,LowerIdent,CloseSquare,OpenCurly,LowerIdent,OpColon,KwPlatform,StringStart,StringPart,StringEnd,CloseCurly,
LowerIdent,OpColon,LowerIdent,OpArrow,LowerIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,LowerIdent,
LowerIdent,OpAssign,OpBar,Underscore,OpBar,OpenCurly,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,Int,CloseRound,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,StringStart,StringPart,StringEnd,CloseRound,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,OpenSquare,Int,Comma,Int,Comma,Int,CloseSquare,CloseRound,
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
		(s-type-anno (name "identity")
			(ty-fn
				(ty-var (raw "a"))
				(ty-var (raw "a"))))
		(s-decl
			(p-ident (raw "identity"))
			(e-lambda
				(args
					(p-ident (raw "x")))
				(e-ident (raw "x"))))
		(s-decl
			(p-ident (raw "main!"))
			(e-lambda
				(args
					(p-underscore))
				(e-block
					(statements
						(s-decl
							(p-ident (raw "num"))
							(e-apply
								(e-ident (raw "identity"))
								(e-int (raw "42"))))
						(s-decl
							(p-ident (raw "str"))
							(e-apply
								(e-ident (raw "identity"))
								(e-string
									(e-string-part (raw "hello")))))
						(s-decl
							(p-ident (raw "lst"))
							(e-apply
								(e-ident (raw "identity"))
								(e-list
									(e-int (raw "1"))
									(e-int (raw "2"))
									(e-int (raw "3")))))
						(e-record)))))))
~~~
# FORMATTED
~~~roc
app [main!] { pf: platform "../basic-cli/platform.roc" }

# Polymorphic identity function with rigid type variable 'a'
identity : a -> a
identity = |x| x

# Use identity at different call sites with different types
main! = |_| {
	# First call with number
	num = identity(42)

	# Second call with string
	str = identity("hello")

	# Third call with list
	lst = identity([1, 2, 3])

	{}
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "identity"))
		(e-lambda
			(args
				(p-assign (ident "x")))
			(e-lookup-local
				(p-assign (ident "x"))))
		(annotation
			(ty-fn (effectful false)
				(ty-rigid-var (name "a"))
				(ty-rigid-var-lookup (ty-rigid-var (name "a"))))))
	(d-let
		(p-assign (ident "main!"))
		(e-closure
			(captures
				(capture (ident "identity")))
			(e-lambda
				(args
					(p-underscore))
				(e-block
					(s-let
						(p-assign (ident "num"))
						(e-call
							(e-lookup-local
								(p-assign (ident "identity")))
							(e-num (value "42"))))
					(s-let
						(p-assign (ident "str"))
						(e-call
							(e-lookup-local
								(p-assign (ident "identity")))
							(e-string
								(e-literal (string "hello")))))
					(s-let
						(p-assign (ident "lst"))
						(e-call
							(e-lookup-local
								(p-assign (ident "identity")))
							(e-list
								(elems
									(e-num (value "1"))
									(e-num (value "2"))
									(e-num (value "3"))))))
					(e-empty_record))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "a -> a"))
		(patt (type "_arg -> {}")))
	(expressions
		(expr (type "a -> a"))
		(expr (type "_arg -> {}"))))
~~~
