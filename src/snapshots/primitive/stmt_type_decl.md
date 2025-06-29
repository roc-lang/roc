# META
~~~ini
description=A primitive
type=file
~~~
# SOURCE
~~~roc
module [Foo]

Foo(a,b) : (a,b,Str,U64)
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),UpperIdent(1:9-1:12),CloseSquare(1:12-1:13),Newline(1:1-1:1),
Newline(1:1-1:1),
UpperIdent(3:1-3:4),NoSpaceOpenRound(3:4-3:5),LowerIdent(3:5-3:6),Comma(3:6-3:7),LowerIdent(3:7-3:8),CloseRound(3:8-3:9),OpColon(3:10-3:11),OpenRound(3:12-3:13),LowerIdent(3:13-3:14),Comma(3:14-3:15),LowerIdent(3:15-3:16),Comma(3:16-3:17),UpperIdent(3:17-3:20),Comma(3:20-3:21),UpperIdent(3:21-3:24),CloseRound(3:24-3:25),EndOfFile(3:25-3:25),
~~~
# PARSE
~~~clojure
(file @1.1-3.25
	(module @1.1-1.13
		(exposes @1.8-1.13
			(exposed-upper-ident (text "Foo"))))
	(statements
		(s-type-decl @3.1-3.25
			(header @3.1-3.9 (name "Foo")
				(args
					(ty-var @3.5-3.6 (raw "a"))
					(ty-var @3.7-3.8 (raw "b"))))
			(ty-tuple @3.12-3.25
				(ty-var @3.13-3.14 (raw "a"))
				(ty-var @3.15-3.16 (raw "b"))
				(ty (name "Str"))
				(ty (name "U64"))))))
~~~
# FORMATTED
~~~roc
module [Foo]

Foo(a, b) : (a, b, Str, U64)
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(s-type-decl @3.1-3.25 (id 81)
		(ty-header @3.1-3.9 (name "Foo")
			(ty-args
				(ty-var @3.5-3.6 (name "a"))
				(ty-var @3.7-3.8 (name "b"))))
		(ty-tuple @3.12-3.25
			(ty-var @3.13-3.14 (name "a"))
			(ty-var @3.15-3.16 (name "b"))
			(ty @3.17-3.20 (name "Str"))
			(ty @3.21-3.24 (name "U64")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(expressions))
~~~
