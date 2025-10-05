# META
~~~ini
description=Using qualified types in pattern matching and function parameters
type=file:Foo.roc
~~~
# SOURCE
~~~roc
Foo := [Whatever].{
    Result := [Success(U64), Failure(Str)]
}

handleSuccess : Foo.Result -> Str
handleSuccess = |res| "success"
~~~
# EXPECTED
UNUSED VARIABLE - nominal_associated_in_patterns.md:6:18:6:21
# PROBLEMS
**UNUSED VARIABLE**
Variable `res` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_res` to suppress this warning.
The unused variable is declared here:
**nominal_associated_in_patterns.md:6:18:6:21:**
```roc
handleSuccess = |res| "success"
```
                 ^^^


# TOKENS
~~~zig
UpperIdent(1:1-1:4),OpColonEqual(1:5-1:7),OpenSquare(1:8-1:9),UpperIdent(1:9-1:17),CloseSquare(1:17-1:18),Dot(1:18-1:19),OpenCurly(1:19-1:20),
UpperIdent(2:5-2:11),OpColonEqual(2:12-2:14),OpenSquare(2:15-2:16),UpperIdent(2:16-2:23),NoSpaceOpenRound(2:23-2:24),UpperIdent(2:24-2:27),CloseRound(2:27-2:28),Comma(2:28-2:29),UpperIdent(2:30-2:37),NoSpaceOpenRound(2:37-2:38),UpperIdent(2:38-2:41),CloseRound(2:41-2:42),CloseSquare(2:42-2:43),
CloseCurly(3:1-3:2),
LowerIdent(5:1-5:14),OpColon(5:15-5:16),UpperIdent(5:17-5:20),NoSpaceDotUpperIdent(5:20-5:27),OpArrow(5:28-5:30),UpperIdent(5:31-5:34),
LowerIdent(6:1-6:14),OpAssign(6:15-6:16),OpBar(6:17-6:18),LowerIdent(6:18-6:21),OpBar(6:21-6:22),StringStart(6:23-6:24),StringPart(6:24-6:31),StringEnd(6:31-6:32),
EndOfFile(7:1-7:1),
~~~
# PARSE
~~~clojure
(file @1.1-6.32
	(type-module @1.1-1.4)
	(statements
		(s-type-decl @1.1-3.2
			(header @1.1-1.4 (name "Foo")
				(args))
			(ty-tag-union @1.8-1.18
				(tags
					(ty @1.9-1.17 (name "Whatever"))))
			(associated @1.19-3.2
				(s-type-decl @2.5-2.43
					(header @2.5-2.11 (name "Result")
						(args))
					(ty-tag-union @2.15-2.43
						(tags
							(ty-apply @2.16-2.28
								(ty @2.16-2.23 (name "Success"))
								(ty @2.24-2.27 (name "U64")))
							(ty-apply @2.30-2.42
								(ty @2.30-2.37 (name "Failure"))
								(ty @2.38-2.41 (name "Str"))))))))
		(s-type-anno @5.1-5.34 (name "handleSuccess")
			(ty-fn @5.17-5.34
				(ty @5.17-5.27 (name "Foo.Result"))
				(ty @5.31-5.34 (name "Str"))))
		(s-decl @6.1-6.32
			(p-ident @6.1-6.14 (raw "handleSuccess"))
			(e-lambda @6.17-6.32
				(args
					(p-ident @6.18-6.21 (raw "res")))
				(e-string @6.23-6.32
					(e-string-part @6.24-6.31 (raw "success")))))))
~~~
# FORMATTED
~~~roc
Foo := [Whatever].{
	Result := [Success(U64), Failure(Str)]
}

handleSuccess : Foo.Result -> Str
handleSuccess = |res| "success"
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @6.1-6.14 (ident "handleSuccess"))
		(e-lambda @6.17-6.32
			(args
				(p-assign @6.18-6.21 (ident "res")))
			(e-string @6.23-6.32
				(e-literal @6.24-6.31 (string "success"))))
		(annotation @6.1-6.14
			(declared-type
				(ty-fn @5.17-5.34 (effectful false)
					(ty-lookup @5.17-5.27 (name "Foo.Result") (local))
					(ty-lookup @5.31-5.34 (name "Str") (builtin))))))
	(s-nominal-decl @1.1-3.2
		(ty-header @1.1-1.4 (name "Foo"))
		(ty-tag-union @1.8-1.18
			(ty-tag-name @1.9-1.17 (name "Whatever"))))
	(s-nominal-decl @2.5-2.43
		(ty-header @2.5-2.11 (name "Result"))
		(ty-tag-union @2.15-2.43
			(ty-tag-name @2.16-2.28 (name "Success")
				(ty-lookup @2.24-2.27 (name "U64") (builtin)))
			(ty-tag-name @2.30-2.42 (name "Failure")
				(ty-lookup @2.38-2.41 (name "Str") (builtin))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @6.1-6.14 (type "Result -> Str")))
	(type_decls
		(nominal @1.1-3.2 (type "Foo")
			(ty-header @1.1-1.4 (name "Foo")))
		(nominal @2.5-2.43 (type "Result")
			(ty-header @2.5-2.11 (name "Result"))))
	(expressions
		(expr @6.17-6.32 (type "Result -> Str"))))
~~~
