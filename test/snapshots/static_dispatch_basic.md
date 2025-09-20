# META
~~~ini
description=Basic static dispatch syntax
type=expr
~~~
# SOURCE
~~~roc
foo.bar(a, b, c)
~~~
# EXPECTED
NIL
# PROBLEMS
**NOT IMPLEMENTED**
This feature is not yet implemented: canonicalize static_dispatch expression

This error doesn't have a proper diagnostic report yet. Let us know if you want to help improve Roc's error messages!

# TOKENS
~~~zig
LowerIdent(1:1-1:4),NoSpaceDotLowerIdent(1:4-1:8),NoSpaceOpenRound(1:8-1:9),LowerIdent(1:9-1:10),Comma(1:10-1:11),LowerIdent(1:12-1:13),Comma(1:13-1:14),LowerIdent(1:15-1:16),CloseRound(1:16-1:17),
EndOfFile(2:1-2:1),
~~~
# PARSE
~~~clojure
(e-static-dispatch @1.1-1.17
	subject
	(e-ident @1.1-1.4 (raw "foo"))
	method
	".bar"
	args
	(e-ident @1.9-1.10 (raw "a"))
	(e-ident @1.12-1.13 (raw "b"))
	(e-ident @1.15-1.16 (raw "c")))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-runtime-error (tag "not_implemented"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.1 (type "Error"))
~~~
