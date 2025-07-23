# META
~~~ini
description=if_then_else (13)
type=expr
~~~
# SOURCE
~~~roc
if # Comment after if
	bool # Comment after cond
		{ # Comment after then open
			1
		} else {
			2
		}
~~~
# EXPECTED
UNDEFINED VARIABLE - if_then_else_multi_comments.md:2:2:2:6
# PROBLEMS
**UNDEFINED VARIABLE**

**Undefined Variable**
The variable 'bool' is not defined:
**if_then_else_multi_comments.md:2:2:2:6:**
```roc
	bool # Comment after cond
```
 ^^^^


# TOKENS
~~~zig
KwIf(1:1-1:3),
LowerIdent(2:2-2:6),
OpenCurly(3:3-3:4),
Int(4:4-4:5),
CloseCurly(5:3-5:4),KwElse(5:5-5:9),OpenCurly(5:10-5:11),
Int(6:4-6:5),
CloseCurly(7:3-7:4),EndOfFile(7:4-7:4),
~~~
# PARSE
~~~clojure
(e-if-then-else @1.1-7.4
	(e-ident @2.2-2.6 (raw "bool"))
	(e-block @3.3-5.4
		(statements
			(e-int @4.4-4.5 (raw "1"))))
	(e-block @5.10-7.4
		(statements
			(e-int @6.4-6.5 (raw "2")))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-if @1.1-7.4
	(if-branches
		(if-branch
			(e-runtime-error (tag "ident_not_in_scope"))
			(e-block @3.3-5.4
				(e-int @4.4-4.5 (value "1")))))
	(if-else
		(e-block @5.10-7.4
			(e-int @6.4-6.5 (value "2")))))
~~~
# TYPES
~~~clojure
(expr @1.1-7.4 (type "Num(_size)"))
~~~
