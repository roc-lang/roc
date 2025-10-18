# META
~~~ini
description=Lambda parameters with unused variable checking
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

# Lambda with unused parameter - should warn
add : U64 -> U64
add = |unused| 42

# Lambda with underscore parameter that is used - should warn
multiply : U64 -> U64
multiply = |_factor| _factor * 2

# Lambda with unused underscore parameter - should be fine
process : U64 -> U64
process = |_input| 100

# Lambda with used parameter - should be fine
double : U64 -> U64
double = |value| value * 2

main! = |_| {
    result1 = add(5)
    result2 = multiply(3)
    result3 = process(7)
    result4 = double(4)
    result1 + result2 + result3 + result4
}
~~~
# EXPECTED
UNUSED VARIABLE - lambda_parameter_unused.md:5:8:5:14
UNDERSCORE VARIABLE USED - lambda_parameter_unused.md:9:22:9:29
# PROBLEMS
**UNUSED VARIABLE**
Variable `unused` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_unused` to suppress this warning.
The unused variable is declared here:
**lambda_parameter_unused.md:5:8:5:14:**
```roc
add = |unused| 42
```
       ^^^^^^


**UNDERSCORE VARIABLE USED**
Variable `_factor` is prefixed with an underscore but is actually used.

Variables prefixed with `_` are intended to be unused. Remove the underscore prefix: `factor`.

**lambda_parameter_unused.md:9:22:9:29:**
```roc
multiply = |_factor| _factor * 2
```
                     ^^^^^^^


# TOKENS
~~~zig
KwApp,OpenSquare,LowerIdent,CloseSquare,OpenCurly,LowerIdent,OpColon,KwPlatform,StringStart,StringPart,StringEnd,CloseCurly,
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,Int,
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,NamedUnderscore,OpBar,NamedUnderscore,OpStar,Int,
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,NamedUnderscore,OpBar,Int,
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,LowerIdent,OpStar,Int,
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
		(s-type-anno (name "add")
			(ty-fn
				(ty (name "U64"))
				(ty (name "U64"))))
		(s-decl
			(p-ident (raw "add"))
			(e-lambda
				(args
					(p-ident (raw "unused")))
				(e-int (raw "42"))))
		(s-type-anno (name "multiply")
			(ty-fn
				(ty (name "U64"))
				(ty (name "U64"))))
		(s-decl
			(p-ident (raw "multiply"))
			(e-lambda
				(args
					(p-ident (raw "_factor")))
				(e-binop (op "*")
					(e-ident (raw "_factor"))
					(e-int (raw "2")))))
		(s-type-anno (name "process")
			(ty-fn
				(ty (name "U64"))
				(ty (name "U64"))))
		(s-decl
			(p-ident (raw "process"))
			(e-lambda
				(args
					(p-ident (raw "_input")))
				(e-int (raw "100"))))
		(s-type-anno (name "double")
			(ty-fn
				(ty (name "U64"))
				(ty (name "U64"))))
		(s-decl
			(p-ident (raw "double"))
			(e-lambda
				(args
					(p-ident (raw "value")))
				(e-binop (op "*")
					(e-ident (raw "value"))
					(e-int (raw "2")))))
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
								(e-ident (raw "add"))
								(e-int (raw "5"))))
						(s-decl
							(p-ident (raw "result2"))
							(e-apply
								(e-ident (raw "multiply"))
								(e-int (raw "3"))))
						(s-decl
							(p-ident (raw "result3"))
							(e-apply
								(e-ident (raw "process"))
								(e-int (raw "7"))))
						(s-decl
							(p-ident (raw "result4"))
							(e-apply
								(e-ident (raw "double"))
								(e-int (raw "4"))))
						(e-binop (op "+")
							(e-binop (op "+")
								(e-binop (op "+")
									(e-ident (raw "result1"))
									(e-ident (raw "result2")))
								(e-ident (raw "result3")))
							(e-ident (raw "result4")))))))))
~~~
# FORMATTED
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

# Lambda with unused parameter - should warn
add : U64 -> U64
add = |unused| 42

# Lambda with underscore parameter that is used - should warn
multiply : U64 -> U64
multiply = |_factor| _factor * 2

# Lambda with unused underscore parameter - should be fine
process : U64 -> U64
process = |_input| 100

# Lambda with used parameter - should be fine
double : U64 -> U64
double = |value| value * 2

main! = |_| {
	result1 = add(5)
	result2 = multiply(3)
	result3 = process(7)
	result4 = double(4)
	result1 + result2 + result3 + result4
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "add"))
		(e-lambda
			(args
				(p-assign (ident "unused")))
			(e-num (value "42")))
		(annotation
			(declared-type
				(ty-fn (effectful false)
					(ty-lookup (name "U64") (builtin))
					(ty-lookup (name "U64") (builtin))))))
	(d-let
		(p-assign (ident "multiply"))
		(e-lambda
			(args
				(p-assign (ident "_factor")))
			(e-binop (op "mul")
				(e-lookup-local
					(p-assign (ident "_factor")))
				(e-num (value "2"))))
		(annotation
			(declared-type
				(ty-fn (effectful false)
					(ty-lookup (name "U64") (builtin))
					(ty-lookup (name "U64") (builtin))))))
	(d-let
		(p-assign (ident "process"))
		(e-lambda
			(args
				(p-assign (ident "_input")))
			(e-num (value "100")))
		(annotation
			(declared-type
				(ty-fn (effectful false)
					(ty-lookup (name "U64") (builtin))
					(ty-lookup (name "U64") (builtin))))))
	(d-let
		(p-assign (ident "double"))
		(e-lambda
			(args
				(p-assign (ident "value")))
			(e-binop (op "mul")
				(e-lookup-local
					(p-assign (ident "value")))
				(e-num (value "2"))))
		(annotation
			(declared-type
				(ty-fn (effectful false)
					(ty-lookup (name "U64") (builtin))
					(ty-lookup (name "U64") (builtin))))))
	(d-let
		(p-assign (ident "main!"))
		(e-closure
			(captures
				(capture (ident "add"))
				(capture (ident "multiply"))
				(capture (ident "double"))
				(capture (ident "process")))
			(e-lambda
				(args
					(p-underscore))
				(e-block
					(s-let
						(p-assign (ident "result1"))
						(e-call
							(e-lookup-local
								(p-assign (ident "add")))
							(e-num (value "5"))))
					(s-let
						(p-assign (ident "result2"))
						(e-call
							(e-lookup-local
								(p-assign (ident "multiply")))
							(e-num (value "3"))))
					(s-let
						(p-assign (ident "result3"))
						(e-call
							(e-lookup-local
								(p-assign (ident "process")))
							(e-num (value "7"))))
					(s-let
						(p-assign (ident "result4"))
						(e-call
							(e-lookup-local
								(p-assign (ident "double")))
							(e-num (value "4"))))
					(e-binop (op "add")
						(e-binop (op "add")
							(e-binop (op "add")
								(e-lookup-local
									(p-assign (ident "result1")))
								(e-lookup-local
									(p-assign (ident "result2"))))
							(e-lookup-local
								(p-assign (ident "result3"))))
						(e-lookup-local
							(p-assign (ident "result4")))))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Num(Int(Unsigned64)) -> Num(Int(Unsigned64))"))
		(patt (type "Num(Int(Unsigned64)) -> Num(Int(Unsigned64))"))
		(patt (type "Num(Int(Unsigned64)) -> Num(Int(Unsigned64))"))
		(patt (type "Num(Int(Unsigned64)) -> Num(Int(Unsigned64))"))
		(patt (type "_arg -> Num(Int(Unsigned64))")))
	(expressions
		(expr (type "Num(Int(Unsigned64)) -> Num(Int(Unsigned64))"))
		(expr (type "Num(Int(Unsigned64)) -> Num(Int(Unsigned64))"))
		(expr (type "Num(Int(Unsigned64)) -> Num(Int(Unsigned64))"))
		(expr (type "Num(Int(Unsigned64)) -> Num(Int(Unsigned64))"))
		(expr (type "_arg -> Num(Int(Unsigned64))"))))
~~~
