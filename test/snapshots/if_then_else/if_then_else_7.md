# META
~~~ini
description=if_then_else (7)
type=expr
~~~
# SOURCE
~~~roc
if bool {
	1
} else {
	2
}
~~~
# EXPECTED
UNDEFINED VARIABLE - if_then_else_7.md:1:4:1:8
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `bool` in this scope.
Is there an `import` or `exposing` missing up-top?

**if_then_else_7.md:1:4:1:8:**
```roc
if bool {
```
   ^^^^


# TOKENS
~~~zig
KwIf(1:1-1:3),LowerIdent(1:4-1:8),OpenCurly(1:9-1:10),
Int(2:2-2:3),
CloseCurly(3:1-3:2),KwElse(3:3-3:7),OpenCurly(3:8-3:9),
Int(4:2-4:3),
CloseCurly(5:1-5:2),
EndOfFile(6:1-6:1),
~~~
# PARSE
~~~clojure
(e-if-then-else @1.1-5.2
	(e-ident @1.4-1.8 (raw "bool"))
	(e-block @1.9-3.2
		(statements
			(e-int @2.2-2.3 (raw "1"))))
	(e-block @3.8-5.2
		(statements
			(e-int @4.2-4.3 (raw "2")))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-if @1.1-5.2
	(if-branches
		(if-branch
			(e-runtime-error (tag "ident_not_in_scope"))
			(e-block @1.9-3.2
				(e-num @2.2-2.3 (value "1")))))
	(if-else
		(e-block @3.8-5.2
			(e-num @4.2-4.3 (value "2")))))
~~~
# TYPES
~~~clojure
(expr @1.1-5.2 (type "Num(_size)"))
~~~
