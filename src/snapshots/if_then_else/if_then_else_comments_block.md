# META
~~~ini
description=if_then_else (11)
type=expr
~~~
# SOURCE
~~~roc
if # Comment after if
	bool
		{
			1
		} else {
			2
		}
~~~
~~~
# EXPECTED
NIL
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `bool` in this scope.
Is there an `import` or `exposing` missing up-top?

# TOKENS
~~~zig
KwIf(1:1-1:3),Newline(1:5-1:22),
LowerIdent(2:2-2:6),Newline(1:1-1:1),
OpenCurly(3:3-3:4),Newline(1:1-1:1),
Int(4:4-4:5),Newline(1:1-1:1),
CloseCurly(5:3-5:4),KwElse(5:5-5:9),OpenCurly(5:10-5:11),Newline(1:1-1:1),
Int(6:4-6:5),Newline(1:1-1:1),
CloseCurly(7:3-7:4),Newline(1:1-1:1),
MalformedUnknownToken(8:1-8:2),MalformedUnknownToken(8:2-8:3),MalformedUnknownToken(8:3-8:4),EndOfFile(8:4-8:4),
~~~
# PARSE
~~~clojure
(e-if-then-else @1.1-8.2
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
if # Comment after if
	bool
		{
			1
		} else {
			2
		}
~~~
# CANONICALIZE
~~~clojure
(e-if @1.1-8.2
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
(expr @1.1-8.2 (type "Num(*)"))
~~~
