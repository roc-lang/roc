# META
~~~ini
description=Lambda return type constraint bug - integer literals in lambda bodies should be constrained by function signature
type=file
~~~
# SOURCE
~~~roc
app [main] { pf: platform "platform/main.roc" }

helper : I64 -> I64
helper = |n| n * 2

main : I64, I64 -> I64
main = |_, _| helper(5)
~~~
# EXPECTED
TYPE MISMATCH - lambda_ret_constraint_bug.md:7:22:7:23
# PROBLEMS
**TYPE MISMATCH**
The first argument to this function is not what I expect:
**lambda_ret_constraint_bug.md:7:22:7:23:**
```roc
main = |_, _| helper(5)
```
                     ^

This argument is of type:
    _Num(_size)_

But the function needs the first argumument to be:
    _I64_

# TOKENS
~~~zig
KwApp(1:1-1:4),OpenSquare(1:5-1:6),LowerIdent(1:6-1:10),CloseSquare(1:10-1:11),OpenCurly(1:12-1:13),LowerIdent(1:14-1:16),OpColon(1:16-1:17),KwPlatform(1:18-1:26),StringStart(1:27-1:28),StringPart(1:28-1:45),StringEnd(1:45-1:46),CloseCurly(1:47-1:48),
LowerIdent(3:1-3:7),OpColon(3:8-3:9),UpperIdent(3:10-3:13),OpArrow(3:14-3:16),UpperIdent(3:17-3:20),
LowerIdent(4:1-4:7),OpAssign(4:8-4:9),OpBar(4:10-4:11),LowerIdent(4:11-4:12),OpBar(4:12-4:13),LowerIdent(4:14-4:15),OpStar(4:16-4:17),Int(4:18-4:19),
LowerIdent(6:1-6:5),OpColon(6:6-6:7),UpperIdent(6:8-6:11),Comma(6:11-6:12),UpperIdent(6:13-6:16),OpArrow(6:17-6:19),UpperIdent(6:20-6:23),
LowerIdent(7:1-7:5),OpAssign(7:6-7:7),OpBar(7:8-7:9),Underscore(7:9-7:10),Comma(7:10-7:11),Underscore(7:12-7:13),OpBar(7:13-7:14),LowerIdent(7:15-7:21),NoSpaceOpenRound(7:21-7:22),Int(7:22-7:23),CloseRound(7:23-7:24),EndOfFile(7:24-7:24),
~~~
# PARSE
~~~clojure
(file @1.1-7.24
	(app @1.1-1.48
		(provides @1.5-1.11
			(exposed-lower-ident @1.6-1.10
				(text "main")))
		(record-field @1.14-1.46 (name "pf")
			(e-string @1.27-1.46
				(e-string-part @1.28-1.45 (raw "platform/main.roc"))))
		(packages @1.12-1.48
			(record-field @1.14-1.46 (name "pf")
				(e-string @1.27-1.46
					(e-string-part @1.28-1.45 (raw "platform/main.roc"))))))
	(statements
		(s-type-anno @3.1-3.20 (name "helper")
			(ty-fn @3.10-3.20
				(ty @3.10-3.13 (name "I64"))
				(ty @3.17-3.20 (name "I64"))))
		(s-decl @4.1-4.19
			(p-ident @4.1-4.7 (raw "helper"))
			(e-lambda @4.10-4.19
				(args
					(p-ident @4.11-4.12 (raw "n")))
				(e-binop @4.14-4.19 (op "*")
					(e-ident @4.14-4.15 (raw "n"))
					(e-int @4.18-4.19 (raw "2")))))
		(s-type-anno @6.1-6.23 (name "main")
			(ty-fn @6.8-6.23
				(ty @6.8-6.11 (name "I64"))
				(ty @6.13-6.16 (name "I64"))
				(ty @6.20-6.23 (name "I64"))))
		(s-decl @7.1-7.24
			(p-ident @7.1-7.5 (raw "main"))
			(e-lambda @7.8-7.24
				(args
					(p-underscore)
					(p-underscore))
				(e-apply @7.15-7.24
					(e-ident @7.15-7.21 (raw "helper"))
					(e-int @7.22-7.23 (raw "5")))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @4.1-4.7 (ident "helper"))
		(e-lambda @4.10-4.19
			(args
				(p-assign @4.11-4.12 (ident "n")))
			(e-binop @4.14-4.19 (op "mul")
				(e-lookup-local @4.14-4.15
					(p-assign @4.11-4.12 (ident "n")))
				(e-int @4.18-4.19 (value "2"))))
		(annotation @4.1-4.7
			(declared-type
				(ty-fn @3.10-3.20 (effectful false)
					(ty @3.10-3.13 (name "I64"))
					(ty @3.17-3.20 (name "I64"))))))
	(d-let
		(p-assign @7.1-7.5 (ident "main"))
		(e-closure @7.8-7.24
			(captures
				(capture @4.1-4.7 (ident "helper")))
			(e-lambda @7.8-7.24
				(args
					(p-underscore @7.9-7.10)
					(p-underscore @7.12-7.13))
				(e-call @7.15-7.24
					(e-lookup-local @7.15-7.21
						(p-assign @4.1-4.7 (ident "helper")))
					(e-int @7.22-7.23 (value "5")))))
		(annotation @7.1-7.5
			(declared-type
				(ty-fn @6.8-6.23 (effectful false)
					(ty @6.8-6.11 (name "I64"))
					(ty @6.13-6.16 (name "I64"))
					(ty @6.20-6.23 (name "I64")))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @4.1-4.7 (type "Error -> Error"))
		(patt @7.1-7.5 (type "Error, Error -> Error")))
	(expressions
		(expr @4.10-4.19 (type "Error -> Error"))
		(expr @7.8-7.24 (type "Error, Error -> Error"))))
~~~
