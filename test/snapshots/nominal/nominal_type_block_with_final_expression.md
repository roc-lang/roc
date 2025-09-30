# META
~~~ini
description=Nominal type block with final expression produces error
type=file
~~~
# SOURCE
~~~roc
module []

Foo := [A, B, C].{ x = 5
x }
~~~
# EXPECTED
EXPRESSION IN ASSOCIATED ITEMS - nominal_type_block_with_final_expression.md:4:1:4:2
# PROBLEMS
**EXPRESSION IN ASSOCIATED ITEMS**
Associated items (such as types or methods) can only have associated types and values, not plain expressions.

To fix this, remove the expression at the very end.

**nominal_type_block_with_final_expression.md:4:1:4:2:**
```roc
x }
```
^


# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),CloseSquare(1:9-1:10),
UpperIdent(3:1-3:4),OpColonEqual(3:5-3:7),OpenSquare(3:8-3:9),UpperIdent(3:9-3:10),Comma(3:10-3:11),UpperIdent(3:12-3:13),Comma(3:13-3:14),UpperIdent(3:15-3:16),CloseSquare(3:16-3:17),Dot(3:17-3:18),OpenCurly(3:18-3:19),LowerIdent(3:20-3:21),OpAssign(3:22-3:23),Int(3:24-3:25),
LowerIdent(4:1-4:2),CloseCurly(4:3-4:4),
EndOfFile(5:1-5:1),
~~~
# PARSE
~~~clojure
(file @1.1-4.4
	(module @1.1-1.10
		(exposes @1.8-1.10))
	(statements
		(s-type-decl @3.1-4.4
			(header @3.1-3.4 (name "Foo")
				(args))
			(ty-tag-union @3.8-3.17
				(tags
					(ty @3.9-3.10 (name "A"))
					(ty @3.12-3.13 (name "B"))
					(ty @3.15-3.16 (name "C")))))))
~~~
# FORMATTED
~~~roc
module []

Foo := [A, B, C]
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(s-nominal-decl @3.1-4.4
		(ty-header @3.1-3.4 (name "Foo"))
		(ty-tag-union @3.8-3.17
			(ty @3.9-3.10 (name "A"))
			(ty @3.12-3.13 (name "B"))
			(ty @3.15-3.16 (name "C")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(type_decls
		(nominal @3.1-4.4 (type "Foo")
			(ty-header @3.1-3.4 (name "Foo"))))
	(expressions))
~~~
