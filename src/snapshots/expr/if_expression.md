# META
~~~ini
description=If expression with conditional
type=expr
~~~
# SOURCE
~~~roc
if x > 5 "big" else "small"
~~~
# EXPECTED
UNDEFINED VARIABLE - if_expression.md:1:4:1:5
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `x` in this scope.
Is there an `import` or `exposing` missing up-top?

**if_expression.md:1:4:1:5:**
```roc
if x > 5 "big" else "small"
```
   ^


# TOKENS
~~~zig
KwIf(1:1-1:3),LowerIdent(1:4-1:5),OpGreaterThan(1:6-1:7),Int(1:8-1:9),StringStart(1:10-1:11),StringPart(1:11-1:14),StringEnd(1:14-1:15),KwElse(1:16-1:20),StringStart(1:21-1:22),StringPart(1:22-1:27),StringEnd(1:27-1:28),EndOfFile(1:28-1:28),
~~~
# PARSE
~~~clojure
(e-if-then-else @1.1-1.28
	(e-binop @1.4-1.9 (op ">")
		(e-ident @1.4-1.5 (raw "x"))
		(e-int @1.8-1.9 (raw "5")))
	(e-string @1.10-1.15
		(e-string-part @1.11-1.14 (raw "big")))
	(e-string @1.21-1.28
		(e-string-part @1.22-1.27 (raw "small"))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-if @1.1-1.28
	(if-branches
		(if-branch
			(e-binop @1.4-1.9 (op "gt")
				(e-runtime-error (tag "ident_not_in_scope"))
				(e-int @1.8-1.9 (value "5")))
			(e-string @1.10-1.15
				(e-literal @1.11-1.14 (string "big")))))
	(if-else
		(e-string @1.21-1.28
			(e-literal @1.22-1.27 (string "small")))))
~~~
# TYPES
~~~clojure
(expr @1.1-1.28 (type "Str"))
~~~
