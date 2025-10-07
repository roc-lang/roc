# META
~~~ini
description=Nominal type associated items with final expression produces error
type=snippet
~~~
# SOURCE
~~~roc
Foo := [A, B, C].{ x = 5
x }
~~~
# EXPECTED
EXPRESSION IN ASSOCIATED ITEMS - nominal_associated_with_final_expression.md:2:1:2:2
UNUSED VARIABLE - nominal_associated_with_final_expression.md:1:20:1:25
# PROBLEMS
**EXPRESSION IN ASSOCIATED ITEMS**
Associated items (such as types or methods) can only have associated types and values, not plain expressions.

To fix this, remove the expression at the very end.

**nominal_associated_with_final_expression.md:2:1:2:2:**
```roc
x }
```
^


**UNUSED VARIABLE**
Variable `x` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_x` to suppress this warning.
The unused variable is declared here:
**nominal_associated_with_final_expression.md:1:20:1:25:**
```roc
Foo := [A, B, C].{ x = 5
```
                   ^^^^^


# TOKENS
~~~zig
UpperIdent(1:1-1:4),OpColonEqual(1:5-1:7),OpenSquare(1:8-1:9),UpperIdent(1:9-1:10),Comma(1:10-1:11),UpperIdent(1:12-1:13),Comma(1:13-1:14),UpperIdent(1:15-1:16),CloseSquare(1:16-1:17),Dot(1:17-1:18),OpenCurly(1:18-1:19),LowerIdent(1:20-1:21),OpAssign(1:22-1:23),Int(1:24-1:25),
LowerIdent(2:1-2:2),CloseCurly(2:3-2:4),
EndOfFile(3:1-3:1),
~~~
# PARSE
~~~clojure
(file @1.1-2.4
	(type-module @1.1-1.4)
	(statements
		(s-type-decl @1.1-2.4
			(header @1.1-1.4 (name "Foo")
				(args))
			(ty-tag-union @1.8-1.17
				(tags
					(ty @1.9-1.10 (name "A"))
					(ty @1.12-1.13 (name "B"))
					(ty @1.15-1.16 (name "C"))))
			(associated @1.18-2.4
				(s-decl @1.20-1.25
					(p-ident @1.20-1.21 (raw "x"))
					(e-int @1.24-1.25 (raw "5")))
				(e-ident @2.1-2.2 (raw "x"))))))
~~~
# FORMATTED
~~~roc
Foo := [A, B, C].{
	x = 5
	x
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @1.20-1.25 (ident "Foo.x"))
		(e-num @1.24-1.25 (value "5")))
	(s-nominal-decl @1.1-2.4
		(ty-header @1.1-1.4 (name "Foo"))
		(ty-tag-union @1.8-1.17
			(ty-tag-name @1.9-1.10 (name "A"))
			(ty-tag-name @1.12-1.13 (name "B"))
			(ty-tag-name @1.15-1.16 (name "C")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @1.20-1.25 (type "Num(_size)")))
	(type_decls
		(nominal @1.1-2.4 (type "Foo")
			(ty-header @1.1-1.4 (name "Foo"))))
	(expressions
		(expr @1.24-1.25 (type "Num(_size)"))))
~~~
