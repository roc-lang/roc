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
# EXPECTED
UNUSED VARIABLE - block_pattern_unify.md:3:5:3:8
# PROBLEMS
**UNUSED VARIABLE**
Variable `str` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_str` to suppress this warning.
The unused variable is declared here:
**block_pattern_unify.md:3:5:3:8:**
```roc
    str = "hello"
```
    ^^^


# TOKENS
~~~zig
OpenCurly(1:1-1:2),
LowerIdent(2:5-2:6),OpAssign(2:7-2:8),Int(2:9-2:11),
LowerIdent(3:5-3:8),OpAssign(3:9-3:10),StringStart(3:11-3:12),StringPart(3:12-3:17),StringEnd(3:17-3:18),
LowerIdent(4:5-4:11),OpAssign(4:12-4:13),LowerIdent(4:14-4:15),OpPlus(4:16-4:17),Int(4:18-4:19),
LowerIdent(5:5-5:11),
CloseCurly(6:1-6:2),
EndOfFile(7:1-7:1),
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
		(s-decl @4.5-4.19
			(p-ident @4.5-4.11 (raw "result"))
			(e-binop @4.14-4.19 (op "+")
				(e-ident @4.14-4.15 (raw "x"))
				(e-int @4.18-4.19 (raw "5"))))
		(e-ident @5.5-5.11 (raw "result"))))
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
(e-block @1.1-6.2
	(s-let @2.5-2.11
		(p-assign @2.5-2.6 (ident "x"))
		(e-int @2.9-2.11 (value "42")))
	(s-let @3.5-3.18
		(p-assign @3.5-3.8 (ident "str"))
		(e-string @3.11-3.18
			(e-literal @3.12-3.17 (string "hello"))))
	(s-let @4.5-4.19
		(p-assign @4.5-4.11 (ident "result"))
		(e-binop @4.14-4.19 (op "add")
			(e-lookup-local @4.14-4.15
				(p-assign @2.5-2.6 (ident "x")))
			(e-int @4.18-4.19 (value "5"))))
	(e-lookup-local @5.5-5.11
		(p-assign @4.5-4.11 (ident "result"))))
~~~
# TYPES
~~~clojure
(expr @1.1-6.2 (type "Num(_size)"))
~~~
