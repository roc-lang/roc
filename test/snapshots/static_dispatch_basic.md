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
UNDEFINED VARIABLE - static_dispatch_basic.md:1:1:1:4
UNDEFINED VARIABLE - static_dispatch_basic.md:1:9:1:10
UNDEFINED VARIABLE - static_dispatch_basic.md:1:12:1:13
UNDEFINED VARIABLE - static_dispatch_basic.md:1:15:1:16
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `foo` in this scope.
Is there an `import` or `exposing` missing up-top?

**static_dispatch_basic.md:1:1:1:4:**
```roc
foo.bar(a, b, c)
```
^^^


**UNDEFINED VARIABLE**
Nothing is named `a` in this scope.
Is there an `import` or `exposing` missing up-top?

**static_dispatch_basic.md:1:9:1:10:**
```roc
foo.bar(a, b, c)
```
        ^


**UNDEFINED VARIABLE**
Nothing is named `b` in this scope.
Is there an `import` or `exposing` missing up-top?

**static_dispatch_basic.md:1:12:1:13:**
```roc
foo.bar(a, b, c)
```
           ^


**UNDEFINED VARIABLE**
Nothing is named `c` in this scope.
Is there an `import` or `exposing` missing up-top?

**static_dispatch_basic.md:1:15:1:16:**
```roc
foo.bar(a, b, c)
```
              ^


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
	"bar"
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
(e-dot-access @1.1-1.17 (field "bar")
	(receiver
		(e-runtime-error (tag "ident_not_in_scope")))
	(args
		(e-runtime-error (tag "ident_not_in_scope"))
		(e-runtime-error (tag "ident_not_in_scope"))
		(e-runtime-error (tag "ident_not_in_scope"))))
~~~
# TYPES
~~~clojure
(expr @1.1-1.17 (type "_d"))
~~~
