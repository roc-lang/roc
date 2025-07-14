# META
~~~ini
description=if_then_else (15)
type=expr
~~~
# SOURCE
~~~roc
if # Comment after if
	bool # Comment after cond
		{ # Comment after then open
			1
		} # Comment after then close
			else # Comment after else
				{ # Comment else open
					2
				}
~~~
# EXPECTED
UNDEFINED VARIABLE - if_then_else_comments_complex.md:2:2:2:6
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `bool` in this scope.
Is there an `import` or `exposing` missing up-top?

**if_then_else_comments_complex.md:2:2:2:6:**
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
CloseCurly(5:3-5:4),
KwElse(6:4-6:8),
OpenCurly(7:5-7:6),
Int(8:6-8:7),
CloseCurly(9:5-9:6),EndOfFile(9:6-9:6),
~~~
# PARSE
~~~clojure
(e-if-then-else @1.1-9.6
	(e-ident @2.2-2.6 (raw "bool"))
	(e-block @3.3-5.4
		(statements
			(e-int @4.4-4.5 (raw "1"))))
	(e-block @7.5-9.6
		(statements
			(e-int @8.6-8.7 (raw "2")))))
~~~
# FORMATTED
~~~roc
if # Comment after if
	bool # Comment after cond
		{ # Comment after then open
			1 # Comment after then close
		} # Comment after then close
			else # Comment after else
				{ # Comment else open
					2
				}
~~~
# CANONICALIZE
~~~clojure
(e-if @1.1-9.6
	(if-branches
		(if-branch
			(e-runtime-error (tag "ident_not_in_scope"))
			(e-block @3.3-5.4
				(e-int @4.4-4.5 (value "1")))))
	(if-else
		(e-block @7.5-9.6
			(e-int @8.6-8.7 (value "2")))))
~~~
# TYPES
~~~clojure
(expr @1.1-9.6 (type "Num(_size)"))
~~~
