# META
~~~ini
description=equals
type=expr
~~~
# SOURCE
~~~roc
x==y
~~~
# EXPECTED
UNDEFINED VARIABLE - equals.md:1:1:1:2
UNDEFINED VARIABLE - equals.md:1:4:1:5
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `x` in this scope.
Is there an `import` or `exposing` missing up-top?

**equals.md:1:1:1:2:**
```roc
x==y
```
^


**UNDEFINED VARIABLE**
Nothing is named `y` in this scope.
Is there an `import` or `exposing` missing up-top?

**equals.md:1:4:1:5:**
```roc
x==y
```
   ^


# TOKENS
~~~zig
LowerIdent(1:1-1:2),OpEquals(1:2-1:4),LowerIdent(1:4-1:5),EndOfFile(1:5-1:5),
~~~
# PARSE
~~~clojure
(e-binop @1.1-1.5 (op "==")
	(e-ident @1.1-1.2 (raw "x"))
	(e-ident @1.4-1.5 (raw "y")))
~~~
# FORMATTED
~~~roc
x == y
~~~
# CANONICALIZE
~~~clojure
(e-binop @1.1-1.5 (op "eq")
	(e-runtime-error (tag "ident_not_in_scope"))
	(e-runtime-error (tag "ident_not_in_scope")))
~~~
# TYPES
~~~clojure
(expr @1.1-1.5 (type "*"))
~~~
