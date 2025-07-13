# META
~~~ini
description=if_then_else (1)
type=expr
~~~
# SOURCE
~~~roc
if bool 1 else 2
~~~
# EXPECTED
UNDEFINED VARIABLE - if_then_else_simple_minimal.md:1:4:1:8
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `bool` in this scope.
Is there an `import` or `exposing` missing up-top?

**if_then_else_simple_minimal.md:1:4:1:8:**
```roc
if bool 1 else 2
```
   ^^^^


# TOKENS
~~~zig
KwIf(1:1-1:3),LowerIdent(1:4-1:8),Int(1:9-1:10),KwElse(1:11-1:15),Int(1:16-1:17),EndOfFile(1:17-1:17),
~~~
# PARSE
~~~clojure
(e-if-then-else @1.1-1.17
	(e-ident @1.4-1.8 (raw "bool"))
	(e-int @1.9-1.10 (raw "1"))
	(e-int @1.16-1.17 (raw "2")))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-if @1.1-1.17
	(if-branches
		(if-branch
			(e-runtime-error (tag "ident_not_in_scope"))
			(e-int @1.9-1.10 (value "1"))))
	(if-else
		(e-int @1.16-1.17 (value "2"))))
~~~
# TYPES
~~~clojure
(expr @1.1-1.17 (type "Num(size)"))
~~~
