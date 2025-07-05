# META
~~~ini
description=when_in_binops
type=expr
~~~
# SOURCE
~~~roc
di<s<when b
 is 7->7e
 zl
~~~
# EXPECTED
UNDEFINED VARIABLE - when_in_binops.md:1:1:1:3
UNDEFINED VARIABLE - when_in_binops.md:1:4:1:5
UNDEFINED VARIABLE - when_in_binops.md:1:6:1:10
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent(1:1-1:3),OpLessThan(1:3-1:4),LowerIdent(1:4-1:5),OpLessThan(1:5-1:6),LowerIdent(1:6-1:10),LowerIdent(1:11-1:12),Newline(1:1-1:1),
LowerIdent(2:2-2:4),Int(2:5-2:6),OpArrow(2:6-2:8),MalformedNumberNoExponentDigits(2:8-2:10),Newline(1:1-1:1),
LowerIdent(3:2-3:4),EndOfFile(3:4-3:4),
~~~
# PARSE
~~~clojure
(e-binop @1.1-1.12 (op "<")
	(e-ident @1.1-1.3 (raw "di"))
	(e-binop @1.4-1.12 (op "<")
		(e-ident @1.4-1.5 (raw "s"))
		(e-ident @1.6-1.10 (raw "when"))))
~~~
# FORMATTED
~~~roc
di < s < when
~~~
# CANONICALIZE
~~~clojure
(e-binop @1.1-1.12 (op "lt")
	(e-runtime-error (tag "ident_not_in_scope"))
	(e-binop @1.4-1.12 (op "lt")
		(e-runtime-error (tag "ident_not_in_scope"))
		(e-runtime-error (tag "ident_not_in_scope"))))
~~~
# TYPES
~~~clojure
(expr @1.1-1.12 (type "*"))
~~~
