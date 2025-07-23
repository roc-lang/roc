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
TYPE MISMATCH - lambda_parameter_unused.md:24:25:24:42
TYPE MISMATCH - lambda_parameter_unused.md:24:15:24:42
TYPE MISMATCH - lambda_parameter_unused.md:24:5:24:42
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

Variables prefixed with an underscore are supposed to be ignored. But `_factor` is used here:
**lambda_parameter_unused.md:9:22:9:29:**
```roc
multiply = |_factor| _factor * 2
```
                     ^^^^^^^


**TYPE MISMATCH**
This expression is used in an unexpected way:
**lambda_parameter_unused.md:24:25:24:42:**
```roc
    result1 + result2 + result3 + result4
```
                        ^^^^^^^^^^^^^^^^^

It is of type:
    _Num(_size)_

But you are trying to use it as:
    _U64_

**TYPE MISMATCH**
This expression is used in an unexpected way:
**lambda_parameter_unused.md:24:15:24:42:**
```roc
    result1 + result2 + result3 + result4
```
              ^^^^^^^^^^^^^^^^^^^^^^^^^^^

It is of type:
    _Num(_size)_

But you are trying to use it as:
    _U64_

**TYPE MISMATCH**
This expression is used in an unexpected way:
**lambda_parameter_unused.md:24:5:24:42:**
```roc
    result1 + result2 + result3 + result4
```
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

It is of type:
    _Num(_size)_

But you are trying to use it as:
    _U64_

# TOKENS
~~~zig
KwApp(1:1-1:4),OpenSquare(1:5-1:6),LowerIdent(1:6-1:11),CloseSquare(1:11-1:12),OpenCurly(1:13-1:14),LowerIdent(1:15-1:17),OpColon(1:17-1:18),KwPlatform(1:19-1:27),StringStart(1:28-1:29),StringPart(1:29-1:50),StringEnd(1:50-1:51),CloseCurly(1:52-1:53),
LowerIdent(4:1-4:4),OpColon(4:5-4:6),UpperIdent(4:7-4:10),OpArrow(4:11-4:13),UpperIdent(4:14-4:17),
LowerIdent(5:1-5:4),OpAssign(5:5-5:6),OpBar(5:7-5:8),LowerIdent(5:8-5:14),OpBar(5:14-5:15),Int(5:16-5:18),
LowerIdent(8:1-8:9),OpColon(8:10-8:11),UpperIdent(8:12-8:15),OpArrow(8:16-8:18),UpperIdent(8:19-8:22),
LowerIdent(9:1-9:9),OpAssign(9:10-9:11),OpBar(9:12-9:13),NamedUnderscore(9:13-9:20),OpBar(9:20-9:21),NamedUnderscore(9:22-9:29),OpStar(9:30-9:31),Int(9:32-9:33),
LowerIdent(12:1-12:8),OpColon(12:9-12:10),UpperIdent(12:11-12:14),OpArrow(12:15-12:17),UpperIdent(12:18-12:21),
LowerIdent(13:1-13:8),OpAssign(13:9-13:10),OpBar(13:11-13:12),NamedUnderscore(13:12-13:18),OpBar(13:18-13:19),Int(13:20-13:23),
LowerIdent(16:1-16:7),OpColon(16:8-16:9),UpperIdent(16:10-16:13),OpArrow(16:14-16:16),UpperIdent(16:17-16:20),
LowerIdent(17:1-17:7),OpAssign(17:8-17:9),OpBar(17:10-17:11),LowerIdent(17:11-17:16),OpBar(17:16-17:17),LowerIdent(17:18-17:23),OpStar(17:24-17:25),Int(17:26-17:27),
LowerIdent(19:1-19:6),OpAssign(19:7-19:8),OpBar(19:9-19:10),Underscore(19:10-19:11),OpBar(19:11-19:12),OpenCurly(19:13-19:14),
LowerIdent(20:5-20:12),OpAssign(20:13-20:14),LowerIdent(20:15-20:18),NoSpaceOpenRound(20:18-20:19),Int(20:19-20:20),CloseRound(20:20-20:21),
LowerIdent(21:5-21:12),OpAssign(21:13-21:14),LowerIdent(21:15-21:23),NoSpaceOpenRound(21:23-21:24),Int(21:24-21:25),CloseRound(21:25-21:26),
LowerIdent(22:5-22:12),OpAssign(22:13-22:14),LowerIdent(22:15-22:22),NoSpaceOpenRound(22:22-22:23),Int(22:23-22:24),CloseRound(22:24-22:25),
LowerIdent(23:5-23:12),OpAssign(23:13-23:14),LowerIdent(23:15-23:21),NoSpaceOpenRound(23:21-23:22),Int(23:22-23:23),CloseRound(23:23-23:24),
LowerIdent(24:5-24:12),OpPlus(24:13-24:14),LowerIdent(24:15-24:22),OpPlus(24:23-24:24),LowerIdent(24:25-24:32),OpPlus(24:33-24:34),LowerIdent(24:35-24:42),
CloseCurly(25:1-25:2),EndOfFile(25:2-25:2),
~~~
# PARSE
~~~clojure
(file @1.1-25.2
	(app @1.1-1.53
		(provides @1.5-1.12
			(exposed-lower-ident @1.6-1.11
				(text "main!")))
		(record-field @1.15-1.51 (name "pf")
			(e-string @1.28-1.51
				(e-string-part @1.29-1.50 (raw "../basic-cli/main.roc"))))
		(packages @1.13-1.53
			(record-field @1.15-1.51 (name "pf")
				(e-string @1.28-1.51
					(e-string-part @1.29-1.50 (raw "../basic-cli/main.roc"))))))
	(statements
		(s-type-anno @4.1-4.17 (name "add")
			(ty-fn @4.7-4.17
				(ty @4.7-4.10 (name "U64"))
				(ty @4.14-4.17 (name "U64"))))
		(s-decl @5.1-5.18
			(p-ident @5.1-5.4 (raw "add"))
			(e-lambda @5.7-5.18
				(args
					(p-ident @5.8-5.14 (raw "unused")))
				(e-int @5.16-5.18 (raw "42"))))
		(s-type-anno @8.1-8.22 (name "multiply")
			(ty-fn @8.12-8.22
				(ty @8.12-8.15 (name "U64"))
				(ty @8.19-8.22 (name "U64"))))
		(s-decl @9.1-9.33
			(p-ident @9.1-9.9 (raw "multiply"))
			(e-lambda @9.12-9.33
				(args
					(p-ident @9.13-9.20 (raw "_factor")))
				(e-binop @9.22-9.33 (op "*")
					(e-ident @9.22-9.29 (raw "_factor"))
					(e-int @9.32-9.33 (raw "2")))))
		(s-type-anno @12.1-12.21 (name "process")
			(ty-fn @12.11-12.21
				(ty @12.11-12.14 (name "U64"))
				(ty @12.18-12.21 (name "U64"))))
		(s-decl @13.1-13.23
			(p-ident @13.1-13.8 (raw "process"))
			(e-lambda @13.11-13.23
				(args
					(p-ident @13.12-13.18 (raw "_input")))
				(e-int @13.20-13.23 (raw "100"))))
		(s-type-anno @16.1-16.20 (name "double")
			(ty-fn @16.10-16.20
				(ty @16.10-16.13 (name "U64"))
				(ty @16.17-16.20 (name "U64"))))
		(s-decl @17.1-17.27
			(p-ident @17.1-17.7 (raw "double"))
			(e-lambda @17.10-17.27
				(args
					(p-ident @17.11-17.16 (raw "value")))
				(e-binop @17.18-17.27 (op "*")
					(e-ident @17.18-17.23 (raw "value"))
					(e-int @17.26-17.27 (raw "2")))))
		(s-decl @19.1-25.2
			(p-ident @19.1-19.6 (raw "main!"))
			(e-lambda @19.9-25.2
				(args
					(p-underscore))
				(e-block @19.13-25.2
					(statements
						(s-decl @20.5-20.21
							(p-ident @20.5-20.12 (raw "result1"))
							(e-apply @20.15-20.21
								(e-ident @20.15-20.18 (raw "add"))
								(e-int @20.19-20.20 (raw "5"))))
						(s-decl @21.5-21.26
							(p-ident @21.5-21.12 (raw "result2"))
							(e-apply @21.15-21.26
								(e-ident @21.15-21.23 (raw "multiply"))
								(e-int @21.24-21.25 (raw "3"))))
						(s-decl @22.5-22.25
							(p-ident @22.5-22.12 (raw "result3"))
							(e-apply @22.15-22.25
								(e-ident @22.15-22.22 (raw "process"))
								(e-int @22.23-22.24 (raw "7"))))
						(s-decl @23.5-23.24
							(p-ident @23.5-23.12 (raw "result4"))
							(e-apply @23.15-23.24
								(e-ident @23.15-23.21 (raw "double"))
								(e-int @23.22-23.23 (raw "4"))))
						(e-binop @24.5-24.42 (op "+")
							(e-ident @24.5-24.12 (raw "result1"))
							(e-binop @24.15-24.42 (op "+")
								(e-ident @24.15-24.22 (raw "result2"))
								(e-binop @24.25-24.42 (op "+")
									(e-ident @24.25-24.32 (raw "result3"))
									(e-ident @24.35-24.42 (raw "result4")))))))))))
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
	(def
		(pattern
			(p-assign @5.1-5.4 (ident "add")))
		(expr
			(e-lambda @5.7-5.18
				(args
					(p-assign @5.8-5.14 (ident "unused")))
				(e-int @5.16-5.18 (value "42"))))
		(annotation
			(annotation
				(type-anno
					(ty-fn @4.7-4.17 (effectful false)
						(ty @4.7-4.10 (name "U64"))
						(ty @4.14-4.17 (name "U64")))))))
	(def
		(pattern
			(p-assign @9.1-9.9 (ident "multiply")))
		(expr
			(e-lambda @9.12-9.33
				(args
					(p-assign @9.13-9.20 (ident "_factor")))
				(e-binop @9.22-9.33 (op "mul")
					(e-lookup-local @9.22-9.29
						(p-assign @9.13-9.20 (ident "_factor")))
					(e-int @9.32-9.33 (value "2")))))
		(annotation
			(annotation
				(type-anno
					(ty-fn @8.12-8.22 (effectful false)
						(ty @8.12-8.15 (name "U64"))
						(ty @8.19-8.22 (name "U64")))))))
	(def
		(pattern
			(p-assign @13.1-13.8 (ident "process")))
		(expr
			(e-lambda @13.11-13.23
				(args
					(p-assign @13.12-13.18 (ident "_input")))
				(e-int @13.20-13.23 (value "100"))))
		(annotation
			(annotation
				(type-anno
					(ty-fn @12.11-12.21 (effectful false)
						(ty @12.11-12.14 (name "U64"))
						(ty @12.18-12.21 (name "U64")))))))
	(def
		(pattern
			(p-assign @17.1-17.7 (ident "double")))
		(expr
			(e-lambda @17.10-17.27
				(args
					(p-assign @17.11-17.16 (ident "value")))
				(e-binop @17.18-17.27 (op "mul")
					(e-lookup-local @17.18-17.23
						(p-assign @17.11-17.16 (ident "value")))
					(e-int @17.26-17.27 (value "2")))))
		(annotation
			(annotation
				(type-anno
					(ty-fn @16.10-16.20 (effectful false)
						(ty @16.10-16.13 (name "U64"))
						(ty @16.17-16.20 (name "U64")))))))
	(def
		(pattern
			(p-assign @19.1-19.6 (ident "main!")))
		(expr
			(e-lambda @19.9-25.2
				(args
					(p-underscore @19.10-19.11))
				(e-block @19.13-25.2
					(s-let @20.5-20.21
						(p-assign @20.5-20.12 (ident "result1"))
						(e-call @20.15-20.21
							(e-lookup-local @20.15-20.18
								(p-assign @5.1-5.4 (ident "add")))
							(e-int @20.19-20.20 (value "5"))))
					(s-let @21.5-21.26
						(p-assign @21.5-21.12 (ident "result2"))
						(e-call @21.15-21.26
							(e-lookup-local @21.15-21.23
								(p-assign @9.1-9.9 (ident "multiply")))
							(e-int @21.24-21.25 (value "3"))))
					(s-let @22.5-22.25
						(p-assign @22.5-22.12 (ident "result3"))
						(e-call @22.15-22.25
							(e-lookup-local @22.15-22.22
								(p-assign @13.1-13.8 (ident "process")))
							(e-int @22.23-22.24 (value "7"))))
					(s-let @23.5-23.24
						(p-assign @23.5-23.12 (ident "result4"))
						(e-call @23.15-23.24
							(e-lookup-local @23.15-23.21
								(p-assign @17.1-17.7 (ident "double")))
							(e-int @23.22-23.23 (value "4"))))
					(e-binop @24.5-24.42 (op "add")
						(e-lookup-local @24.5-24.12
							(p-assign @20.5-20.12 (ident "result1")))
						(e-binop @24.15-24.42 (op "add")
							(e-lookup-local @24.15-24.22
								(p-assign @21.5-21.12 (ident "result2")))
							(e-binop @24.25-24.42 (op "add")
								(e-lookup-local @24.25-24.32
									(p-assign @22.5-22.12 (ident "result3")))
								(e-lookup-local @24.35-24.42
									(p-assign @23.5-23.12 (ident "result4")))))))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @5.1-5.4 (type "U64 -> U64"))
		(patt @9.1-9.9 (type "U64 -> U64"))
		(patt @13.1-13.8 (type "U64 -> U64"))
		(patt @17.1-17.7 (type "U64 -> U64"))
		(patt @19.1-19.6 (type "_arg -> Error")))
	(expressions
		(expr @5.7-5.18 (type "U64 -> U64"))
		(expr @9.12-9.33 (type "U64 -> U64"))
		(expr @13.11-13.23 (type "U64 -> U64"))
		(expr @17.10-17.27 (type "U64 -> U64"))
		(expr @19.9-25.2 (type "_arg -> Error"))))
~~~
