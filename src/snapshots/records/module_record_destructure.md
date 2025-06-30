# META
~~~ini
description=Record destructuring in assignment statement
type=file
~~~
# SOURCE
~~~roc
module [extract_age]

extract_age : { age : U64 } -> U64
extract_age = |person| {
    { age } = person
    age
}
~~~
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **= person** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**module_record_destructure.md:5:13:5:21:**
```roc
    { age } = person
```
            ^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named `age` in this scope.
Is there an `import` or `exposing` missing up-top?

**UNDEFINED VARIABLE**
Nothing is named `age` in this scope.
Is there an `import` or `exposing` missing up-top?

# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),LowerIdent(1:9-1:20),CloseSquare(1:20-1:21),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(3:1-3:12),OpColon(3:13-3:14),OpenCurly(3:15-3:16),LowerIdent(3:17-3:20),OpColon(3:21-3:22),UpperIdent(3:23-3:26),CloseCurly(3:27-3:28),OpArrow(3:29-3:31),UpperIdent(3:32-3:35),Newline(1:1-1:1),
LowerIdent(4:1-4:12),OpAssign(4:13-4:14),OpBar(4:15-4:16),LowerIdent(4:16-4:22),OpBar(4:22-4:23),OpenCurly(4:24-4:25),Newline(1:1-1:1),
OpenCurly(5:5-5:6),LowerIdent(5:7-5:10),CloseCurly(5:11-5:12),OpAssign(5:13-5:14),LowerIdent(5:15-5:21),Newline(1:1-1:1),
LowerIdent(6:5-6:8),Newline(1:1-1:1),
CloseCurly(7:1-7:2),EndOfFile(7:2-7:2),
~~~
# PARSE
~~~clojure
(file @1.1-7.2
	(module @1.1-1.21
		(exposes @1.8-1.21
			(exposed-lower-ident (text "extract_age"))))
	(statements
		(s-type-anno @3.1-4.12 (name "extract_age")
			(ty-fn @3.15-3.35
				(ty-record @3.15-3.28
					(anno-record-field @3.17-3.28 (name "age")
						(ty (name "U64"))))
				(ty (name "U64"))))
		(s-decl @4.1-7.2
			(p-ident @4.1-4.12 (raw "extract_age"))
			(e-lambda @4.15-7.2
				(args
					(p-ident @4.16-4.22 (raw "person")))
				(e-block @4.24-7.2
					(statements
						(e-block @5.5-5.12
							(statements
								(e-ident @5.7-5.10 (qaul "") (raw "age"))))
						(e-malformed @5.13-5.21 (reason "expr_unexpected_token"))
						(e-ident @5.15-5.21 (qaul "") (raw "person"))
						(e-ident @6.5-6.8 (qaul "") (raw "age"))))))))
~~~
# FORMATTED
~~~roc
module [extract_age]

extract_age : { age : U64 } -> U64
extract_age = |person| {
	age
	
	person
	age
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @4.1-4.12 (ident "extract_age"))
		(e-lambda @4.15-7.2
			(args
				(p-assign @4.16-4.22 (ident "person")))
			(e-block @4.24-7.2
				(s-expr @5.5-5.14
					(e-block @5.5-5.12
						(e-runtime-error (tag "ident_not_in_scope"))))
				(s-expr @5.15-6.8
					(e-lookup-local @5.15-5.21
						(pattern @4.16-4.22)))
				(e-runtime-error (tag "ident_not_in_scope"))))
		(annotation @4.1-4.12
			(declared-type
				(ty-fn @3.15-3.35 (effectful false)
					(ty-record @3.15-3.28
						(field (field "age")
							(ty @3.23-3.26 (name "U64"))))
					(ty @3.32-3.35 (name "U64")))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @4.1-4.12 (type "{ age: U64 } -> Error")))
	(expressions
		(expr @4.15-7.2 (type "{ age: U64 } -> Error"))))
~~~
