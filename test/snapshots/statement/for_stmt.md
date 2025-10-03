# META
~~~ini
description=For expression stmt
type=statement
~~~
# SOURCE
~~~roc
for x in ["a", "b", "c"] {
  result = result + x
} 
~~~
# EXPECTED
NIL
# PROBLEMS
**NOT IMPLEMENTED**
This feature is not yet implemented: statement type in block

This error doesn't have a proper diagnostic report yet. Let us know if you want to help improve Roc's error messages!

# TOKENS
~~~zig
KwFor(1:1-1:4),LowerIdent(1:5-1:6),KwIn(1:7-1:9),OpenSquare(1:10-1:11),StringStart(1:11-1:12),StringPart(1:12-1:13),StringEnd(1:13-1:14),Comma(1:14-1:15),StringStart(1:16-1:17),StringPart(1:17-1:18),StringEnd(1:18-1:19),Comma(1:19-1:20),StringStart(1:21-1:22),StringPart(1:22-1:23),StringEnd(1:23-1:24),CloseSquare(1:24-1:25),OpenCurly(1:26-1:27),
LowerIdent(2:3-2:9),OpAssign(2:10-2:11),LowerIdent(2:12-2:18),OpPlus(2:19-2:20),LowerIdent(2:21-2:22),
CloseCurly(3:1-3:2),
EndOfFile(4:1-4:1),
~~~
# PARSE
~~~clojure
(s-for @1.1-3.2
	(p-ident @1.5-1.6 (raw "x"))
	(e-list @1.10-1.25
		(e-string @1.11-1.14
			(e-string-part @1.12-1.13 (raw "a")))
		(e-string @1.16-1.19
			(e-string-part @1.17-1.18 (raw "b")))
		(e-string @1.21-1.24
			(e-string-part @1.22-1.23 (raw "c"))))
	(e-block @1.26-3.2
		(statements
			(s-decl @2.3-2.22
				(p-ident @2.3-2.9 (raw "result"))
				(e-binop @2.12-2.22 (op "+")
					(e-ident @2.12-2.18 (raw "result"))
					(e-ident @2.21-2.22 (raw "x")))))))
~~~
# FORMATTED
~~~roc
for x in ["a", "b", "c"] {
	result = result + x
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(s-runtime-error (tag "not_implemented")))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(expressions))
~~~
