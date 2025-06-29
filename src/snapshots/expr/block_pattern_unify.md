# META
~~~ini
description=Block with pattern unification testing
type=expr
~~~
# SOURCE
~~~roc
{
    x = 42
    str = "hello"
    result = x + 5
    result
}
~~~
# PROBLEMS
**UNUSED VARIABLE**
Variable ``str`` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_str` to suppress this warning.
The unused variable is declared here:
**block_pattern_unify.md:3:5:3:8:**
```roc
    str = "hello"
```
    ^^^


# TOKENS
~~~zig
OpenCurly(1:1-1:2),Newline(1:1-1:1),
LowerIdent(2:5-2:6),OpAssign(2:7-2:8),Int(2:9-2:11),Newline(1:1-1:1),
LowerIdent(3:5-3:8),OpAssign(3:9-3:10),StringStart(3:11-3:12),StringPart(3:12-3:17),StringEnd(3:17-3:18),Newline(1:1-1:1),
LowerIdent(4:5-4:11),OpAssign(4:12-4:13),LowerIdent(4:14-4:15),OpPlus(4:16-4:17),Int(4:18-4:19),Newline(1:1-1:1),
LowerIdent(5:5-5:11),Newline(1:1-1:1),
CloseCurly(6:1-6:2),EndOfFile(6:2-6:2),
~~~
# PARSE
~~~clojure
(e-block @1.1-6.2
	(statements
		(s-decl @2.5-2.11
			(p-ident @2.5-2.6 (raw "x"))
			(e-int @2.9-2.11 (raw "42")))
		(s-decl @3.5-3.18
			(p-ident @3.5-3.8 (raw "str"))
			(e-string @3.11-3.18
				(e-string-part @3.12-3.17 (raw "hello"))))
		(s-decl @4.5-5.11
			(p-ident @4.5-4.11 (raw "result"))
			(e-binop @4.14-5.11 (op "+")
				(e-ident @4.14-4.15 (qaul "") (raw "x"))
				(e-int @4.18-4.19 (raw "5"))))
		(e-ident @5.5-5.11 (qaul "") (raw "result"))))
~~~
# FORMATTED
~~~roc
{
	x = 42
	str = "hello"
	result = x + 5
	result
}
~~~
# CANONICALIZE
~~~clojure
(e-block @1.1-6.2 (id 86)
	(s-let @2.5-2.11
		(p-assign @2.5-2.6 (ident "x") (id 73))
		(e-int @2.9-2.11 (value "42") (id 74)))
	(s-let @3.5-3.18
		(p-assign @3.5-3.8 (ident "str") (id 76))
		(e-string @3.11-3.18 (id 78)
			(e-literal @3.12-3.17 (string "hello"))))
	(s-let @4.5-5.11
		(p-assign @4.5-4.11 (ident "result") (id 80))
		(e-binop @4.14-5.11 (op "add") (id 83)
			(e-lookup-local @4.14-4.15
				(pattern (id 73)))
			(e-int @4.18-4.19 (value "5"))))
	(e-lookup-local @5.5-5.11
		(pattern (id 80))))
~~~
# TYPES
~~~clojure
(expr (id 86) (type "*"))
~~~
