# META
~~~ini
description=Lambda return type constraint bug - integer literals in lambda bodies should be constrained by function signature
type=file
~~~
# SOURCE
~~~roc
app [main] { pf: platform "platform/main.roc" }

main : I64, I64 -> I64
main = |_, _| (|n| n * 2)
~~~
# EXPECTED
NIL
# PROBLEMS
**TYPE MISMATCH**
This expression is used in an unexpected way:
**lambda_ret_constraint_bug.md:3:20:3:23:**
```roc
main : I64, I64 -> I64
```
                   ^^^

It is of type:
    _I64_

But you are trying to use it as:
    _Num(_size) -> Num(_size2)_

# TOKENS
~~~zig
KwApp(1:1-1:4),OpenSquare(1:5-1:6),LowerIdent(1:6-1:10),CloseSquare(1:10-1:11),OpenCurly(1:12-1:13),LowerIdent(1:14-1:16),OpColon(1:16-1:17),KwPlatform(1:18-1:26),StringStart(1:27-1:28),StringPart(1:28-1:45),StringEnd(1:45-1:46),CloseCurly(1:47-1:48),
LowerIdent(3:1-3:5),OpColon(3:6-3:7),UpperIdent(3:8-3:11),Comma(3:11-3:12),UpperIdent(3:13-3:16),OpArrow(3:17-3:19),UpperIdent(3:20-3:23),
LowerIdent(4:1-4:5),OpAssign(4:6-4:7),OpBar(4:8-4:9),Underscore(4:9-4:10),Comma(4:10-4:11),Underscore(4:12-4:13),OpBar(4:13-4:14),OpenRound(4:15-4:16),OpBar(4:16-4:17),LowerIdent(4:17-4:18),OpBar(4:18-4:19),LowerIdent(4:20-4:21),OpStar(4:22-4:23),Int(4:24-4:25),CloseRound(4:25-4:26),EndOfFile(4:26-4:26),
~~~
# PARSE
~~~clojure
(file @1.1-4.26
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
		(s-type-anno @3.1-3.23 (name "main")
			(ty-fn @3.8-3.23
				(ty @3.8-3.11 (name "I64"))
				(ty @3.13-3.16 (name "I64"))
				(ty @3.20-3.23 (name "I64"))))
		(s-decl @4.1-4.26
			(p-ident @4.1-4.5 (raw "main"))
			(e-lambda @4.8-4.26
				(args
					(p-underscore)
					(p-underscore))
				(e-tuple @4.15-4.26
					(e-lambda @4.16-4.25
						(args
							(p-ident @4.17-4.18 (raw "n")))
						(e-binop @4.20-4.25 (op "*")
							(e-ident @4.20-4.21 (raw "n"))
							(e-int @4.24-4.25 (raw "2")))))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @4.1-4.5 (ident "main"))
		(e-lambda @4.8-4.26
			(args
				(p-underscore @4.9-4.10)
				(p-underscore @4.12-4.13))
			(e-lambda @4.16-4.25
				(args
					(p-assign @4.17-4.18 (ident "n")))
				(e-binop @4.20-4.25 (op "mul")
					(e-lookup-local @4.20-4.21
						(p-assign @4.17-4.18 (ident "n")))
					(e-int @4.24-4.25 (value "2")))))
		(annotation @4.1-4.5
			(declared-type
				(ty-fn @3.8-3.23 (effectful false)
					(ty @3.8-3.11 (name "I64"))
					(ty @3.13-3.16 (name "I64"))
					(ty @3.20-3.23 (name "I64")))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @4.1-4.5 (type "Error, Error -> Error")))
	(expressions
		(expr @4.8-4.26 (type "Error, Error -> Error"))))
~~~
