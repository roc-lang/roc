# META
~~~ini
description=annotated_record_destructure
type=expr
~~~
# SOURCE
~~~roc
{ x, y } : Foo
{ x, y } = { x : "foo", y : 3.14 }

x
~~~
~~~
# EXPECTED
NIL
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `x` in this scope.
Is there an `import` or `exposing` missing up-top?

**UNDEFINED VARIABLE**
Nothing is named `y` in this scope.
Is there an `import` or `exposing` missing up-top?

# TOKENS
~~~zig
OpenCurly(1:1-1:2),LowerIdent(1:3-1:4),Comma(1:4-1:5),LowerIdent(1:6-1:7),CloseCurly(1:8-1:9),OpColon(1:10-1:11),UpperIdent(1:12-1:15),Newline(1:1-1:1),
OpenCurly(2:1-2:2),LowerIdent(2:3-2:4),Comma(2:4-2:5),LowerIdent(2:6-2:7),CloseCurly(2:8-2:9),OpAssign(2:10-2:11),OpenCurly(2:12-2:13),LowerIdent(2:14-2:15),OpColon(2:16-2:17),StringStart(2:18-2:19),StringPart(2:19-2:22),StringEnd(2:22-2:23),Comma(2:23-2:24),LowerIdent(2:25-2:26),OpColon(2:27-2:28),Float(2:29-2:33),CloseCurly(2:34-2:35),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(4:1-4:2),Newline(1:1-1:1),
MalformedUnknownToken(5:1-5:2),MalformedUnknownToken(5:2-5:3),MalformedUnknownToken(5:3-5:4),EndOfFile(5:4-5:4),
~~~
# PARSE
~~~clojure
(e-record @1.1-1.9
	(field (field "x") (optional false))
	(field (field "y") (optional false)))
~~~
# FORMATTED
~~~roc
{x, y}
~~~
# CANONICALIZE
~~~clojure
(e-record @1.1-1.9
	(fields
		(field (name "x")
			(e-runtime-error (tag "ident_not_in_scope")))
		(field (name "y")
			(e-runtime-error (tag "ident_not_in_scope")))))
~~~
# TYPES
~~~clojure
(expr @1.1-1.9 (type "{ x: Error, y: Error }"))
~~~
