# META
~~~ini
description=Simple unused and used underscore variable test
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

# Regular variable that is unused - should warn
unused_regular = |x| 42

# Underscore variable that is used - should warn
used_underscore = |_value| _value

# Underscore variable that is unused - should be fine
unused_underscore = |_ignored| 100

# Regular variable that is used - should be fine
used_regular = |number| number + 1

main! = |_| {
    a = unused_regular(5)
    b = used_underscore(10)
    c = unused_underscore(15)
    d = used_regular(20)
    a + b + c + d
}
~~~
# EXPECTED
UNUSED VARIABLE - unused_vars_simple.md:4:19:4:20
UNDERSCORE VARIABLE USED - unused_vars_simple.md:7:28:7:34
# PROBLEMS
**UNUSED VARIABLE**
Variable `x` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_x` to suppress this warning.
The unused variable is declared here:
**unused_vars_simple.md:4:19:4:20:**
```roc
unused_regular = |x| 42
```
                  ^


**UNDERSCORE VARIABLE USED**
Variable `_value` is prefixed with an underscore but is actually used.

Variables prefixed with `_` are intended to be unused. Remove the underscore prefix: `value`.

**unused_vars_simple.md:7:28:7:34:**
```roc
used_underscore = |_value| _value
```
                           ^^^^^^


# TOKENS
~~~zig
KwApp,OpenSquare,LowerIdent,CloseSquare,OpenCurly,LowerIdent,OpColon,KwPlatform,StringStart,StringPart,StringEnd,CloseCurly,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,Int,
LowerIdent,OpAssign,OpBar,NamedUnderscore,OpBar,NamedUnderscore,
LowerIdent,OpAssign,OpBar,NamedUnderscore,OpBar,Int,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,LowerIdent,OpPlus,Int,
LowerIdent,OpAssign,OpBar,Underscore,OpBar,OpenCurly,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,Int,CloseRound,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,Int,CloseRound,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,Int,CloseRound,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,Int,CloseRound,
LowerIdent,OpPlus,LowerIdent,OpPlus,LowerIdent,OpPlus,LowerIdent,
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
				(e-string-part (raw "../basic-cli/main.roc"))))
		(packages
			(record-field (name "pf")
				(e-string
					(e-string-part (raw "../basic-cli/main.roc"))))))
	(statements
		(s-decl
			(p-ident (raw "unused_regular"))
			(e-lambda
				(args
					(p-ident (raw "x")))
				(e-int (raw "42"))))
		(s-decl
			(p-ident (raw "used_underscore"))
			(e-lambda
				(args
					(p-ident (raw "_value")))
				(e-ident (raw "_value"))))
		(s-decl
			(p-ident (raw "unused_underscore"))
			(e-lambda
				(args
					(p-ident (raw "_ignored")))
				(e-int (raw "100"))))
		(s-decl
			(p-ident (raw "used_regular"))
			(e-lambda
				(args
					(p-ident (raw "number")))
				(e-binop (op "+")
					(e-ident (raw "number"))
					(e-int (raw "1")))))
		(s-decl
			(p-ident (raw "main!"))
			(e-lambda
				(args
					(p-underscore))
				(e-block
					(statements
						(s-decl
							(p-ident (raw "a"))
							(e-apply
								(e-ident (raw "unused_regular"))
								(e-int (raw "5"))))
						(s-decl
							(p-ident (raw "b"))
							(e-apply
								(e-ident (raw "used_underscore"))
								(e-int (raw "10"))))
						(s-decl
							(p-ident (raw "c"))
							(e-apply
								(e-ident (raw "unused_underscore"))
								(e-int (raw "15"))))
						(s-decl
							(p-ident (raw "d"))
							(e-apply
								(e-ident (raw "used_regular"))
								(e-int (raw "20"))))
						(e-binop (op "+")
							(e-binop (op "+")
								(e-binop (op "+")
									(e-ident (raw "a"))
									(e-ident (raw "b")))
								(e-ident (raw "c")))
							(e-ident (raw "d")))))))))
~~~
# FORMATTED
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

# Regular variable that is unused - should warn
unused_regular = |x| 42

# Underscore variable that is used - should warn
used_underscore = |_value| _value

# Underscore variable that is unused - should be fine
unused_underscore = |_ignored| 100

# Regular variable that is used - should be fine
used_regular = |number| number + 1

main! = |_| {
	a = unused_regular(5)
	b = used_underscore(10)
	c = unused_underscore(15)
	d = used_regular(20)
	a + b + c + d
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "unused_regular"))
		(e-lambda
			(args
				(p-assign (ident "x")))
			(e-num (value "42"))))
	(d-let
		(p-assign (ident "used_underscore"))
		(e-lambda
			(args
				(p-assign (ident "_value")))
			(e-lookup-local
				(p-assign (ident "_value")))))
	(d-let
		(p-assign (ident "unused_underscore"))
		(e-lambda
			(args
				(p-assign (ident "_ignored")))
			(e-num (value "100"))))
	(d-let
		(p-assign (ident "used_regular"))
		(e-lambda
			(args
				(p-assign (ident "number")))
			(e-binop (op "add")
				(e-lookup-local
					(p-assign (ident "number")))
				(e-num (value "1")))))
	(d-let
		(p-assign (ident "main!"))
		(e-closure
			(captures
				(capture (ident "used_underscore"))
				(capture (ident "unused_regular"))
				(capture (ident "unused_underscore"))
				(capture (ident "used_regular")))
			(e-lambda
				(args
					(p-underscore))
				(e-block
					(s-let
						(p-assign (ident "a"))
						(e-call
							(e-lookup-local
								(p-assign (ident "unused_regular")))
							(e-num (value "5"))))
					(s-let
						(p-assign (ident "b"))
						(e-call
							(e-lookup-local
								(p-assign (ident "used_underscore")))
							(e-num (value "10"))))
					(s-let
						(p-assign (ident "c"))
						(e-call
							(e-lookup-local
								(p-assign (ident "unused_underscore")))
							(e-num (value "15"))))
					(s-let
						(p-assign (ident "d"))
						(e-call
							(e-lookup-local
								(p-assign (ident "used_regular")))
							(e-num (value "20"))))
					(e-binop (op "add")
						(e-binop (op "add")
							(e-binop (op "add")
								(e-lookup-local
									(p-assign (ident "a")))
								(e-lookup-local
									(p-assign (ident "b"))))
							(e-lookup-local
								(p-assign (ident "c"))))
						(e-lookup-local
							(p-assign (ident "d")))))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "_arg -> Num(_size)"))
		(patt (type "e -> e"))
		(patt (type "_arg -> Num(_size)"))
		(patt (type "Num(_size) -> Num(_size2)"))
		(patt (type "_arg -> Num(_size)")))
	(expressions
		(expr (type "_arg -> Num(_size)"))
		(expr (type "e -> e"))
		(expr (type "_arg -> Num(_size)"))
		(expr (type "Num(_size) -> Num(_size2)"))
		(expr (type "_arg -> Num(_size)"))))
~~~
