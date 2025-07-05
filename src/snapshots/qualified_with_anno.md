# META
~~~ini
description=Test qualified tag with type annotation
type=file
~~~
# SOURCE
~~~roc
module [value]

MyType := [TagA, TagB]

value : MyType
value = MyType.TagA
~~~
~~~
# EXPECTED
UNEXPECTED TOKEN IN EXPRESSION - qualified_with_anno.md:7:1:7:3
UNEXPECTED TOKEN IN EXPRESSION - qualified_with_anno.md:7:2:7:4
UNEXPECTED TOKEN IN EXPRESSION - qualified_with_anno.md:7:3:7:4
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **~~** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**qualified_with_anno.md:7:1:7:3:**
```roc
~~~
```
^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **~~** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**qualified_with_anno.md:7:2:7:4:**
```roc
~~~
```
 ^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **~** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**qualified_with_anno.md:7:3:7:4:**
```roc
~~~
```
  ^


**INVALID STATEMENT**
The statement **expression** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**INVALID STATEMENT**
The statement **expression** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**INVALID STATEMENT**
The statement **expression** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),LowerIdent(1:9-1:14),CloseSquare(1:14-1:15),Newline(1:1-1:1),
Newline(1:1-1:1),
UpperIdent(3:1-3:7),OpColonEqual(3:8-3:10),OpenSquare(3:11-3:12),UpperIdent(3:12-3:16),Comma(3:16-3:17),UpperIdent(3:18-3:22),CloseSquare(3:22-3:23),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(5:1-5:6),OpColon(5:7-5:8),UpperIdent(5:9-5:15),Newline(1:1-1:1),
LowerIdent(6:1-6:6),OpAssign(6:7-6:8),UpperIdent(6:9-6:15),NoSpaceDotUpperIdent(6:15-6:20),Newline(1:1-1:1),
MalformedUnknownToken(7:1-7:2),MalformedUnknownToken(7:2-7:3),MalformedUnknownToken(7:3-7:4),EndOfFile(7:4-7:4),
~~~
# PARSE
~~~clojure
(file @1.1-7.4
	(module @1.1-1.15
		(exposes @1.8-1.15
			(exposed-lower-ident (text "value"))))
	(statements
		(s-type-decl @3.1-3.23
			(header @3.1-3.7 (name "MyType")
				(args))
			(ty-tag-union @3.11-3.23
				(tags
					(ty @3.12-3.16 (name "TagA"))
					(ty @3.18-3.22 (name "TagB")))))
		(s-type-anno @1.1-1.1 (name "value")
			(ty @5.9-5.15 (name "MyType")))
		(s-decl @6.1-6.20
			(p-ident @6.1-6.6 (raw "value"))
			(e-tag @6.9-6.20 (raw "MyType.TagA")))
		(e-malformed @7.1-7.3 (reason "expr_unexpected_token"))
		(e-malformed @7.2-7.4 (reason "expr_unexpected_token"))
		(e-malformed @7.3-7.4 (reason "expr_unexpected_token"))))
~~~
# FORMATTED
~~~roc
module [value]

MyType : [TagA, TagB]

value : MyType
value = TagA

~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @6.1-6.6 (ident "value"))
		(e-tag @6.9-6.20 (name "TagA"))
		(annotation @6.1-6.6
			(declared-type
				(ty @5.9-5.15 (name "MyType")))))
	(s-nominal-decl @3.1-3.23 (match "TODO")
		(ty-header @3.1-3.7 (name "MyType"))
		(ty-tag-union @3.11-3.23
			(ty @3.12-3.16 (name "TagA"))
			(ty @3.18-3.22 (name "TagB")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @6.1-6.6 (type "[TagA]*")))
	(expressions
		(expr @6.9-6.20 (type "[TagA]*"))))
~~~
