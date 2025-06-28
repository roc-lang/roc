# META
~~~ini
description=Function application expression
type=expr
~~~
# SOURCE
~~~roc
foo(42, "hello")
~~~
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `foo` in this scope.
Is there an `import` or `exposing` missing up-top?

# TOKENS
~~~zig
LowerIdent(1:1-1:4),NoSpaceOpenRound(1:4-1:5),Int(1:5-1:7),Comma(1:7-1:8),StringStart(1:9-1:10),StringPart(1:10-1:15),StringEnd(1:15-1:16),CloseRound(1:16-1:17),EndOfFile(1:17-1:17),
~~~
# PARSE
~~~clojure
(e-apply @1-1-1-17
	(e-ident @1-1-1-4 (qaul "") (raw "foo"))
	(e-int @1-5-1-7 (raw "42"))
	(e-string @1-9-1-16
		(e-string-part @1-10-1-15 (raw "hello"))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-call @1-1-1-17 (id 77)
	(e-runtime-error (tag "ident_not_in_scope"))
	(e-int @1-5-1-7 (value "42"))
	(e-string @1-9-1-16
		(e-literal @1-10-1-15 (string "hello"))))
~~~
# TYPES
~~~clojure
(expr (id 77) (type "*"))
~~~