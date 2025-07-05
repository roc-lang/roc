# META
~~~ini
description=ann_closed_union
type=expr
~~~
# SOURCE
~~~roc
{
    foo : [True, Perhaps Thing]
    foo = True

    42
}
~~~
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: `expected_ty_close_square_or_comma`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**ann_closed_union.md:2:26:2:32:**
```roc
    foo : [True, Perhaps Thing]
```
                         ^^^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token  is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**ann_closed_union.md:2:31:2:31:**
```roc
    foo : [True, Perhaps Thing]
```
                              


**MALFORMED TYPE**
This type annotation is malformed or contains invalid syntax.

**UNUSED VARIABLE**
Variable ``foo`` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_foo` to suppress this warning.
The unused variable is declared here:
**ann_closed_union.md:3:5:3:8:**
```roc
    foo = True
```
    ^^^


# TOKENS
~~~zig
OpenCurly(1:1-1:2),Newline(1:1-1:1),
LowerIdent(2:5-2:8),OpColon(2:9-2:10),OpenSquare(2:11-2:12),UpperIdent(2:12-2:16),Comma(2:16-2:17),UpperIdent(2:18-2:25),UpperIdent(2:26-2:31),CloseSquare(2:31-2:32),Newline(1:1-1:1),
LowerIdent(3:5-3:8),OpAssign(3:9-3:10),UpperIdent(3:11-3:15),Newline(1:1-1:1),
Newline(1:1-1:1),
Int(5:5-5:7),Newline(1:1-1:1),
CloseCurly(6:1-6:2),EndOfFile(6:2-6:2),
~~~
# PARSE
~~~clojure
(e-block @1.1-6.2
	(statements
		(s-type-anno @2.5-2.32 (name "foo")
			(ty-malformed @2.26-2.32 (tag "expected_ty_close_square_or_comma")))
		(e-malformed @1.1-1.1 (reason "expr_unexpected_token"))
		(s-decl @3.5-3.15
			(p-ident @3.5-3.8 (raw "foo"))
			(e-tag @3.11-3.15 (raw "True")))
		(e-int @5.5-5.7 (raw "42"))))
~~~
# FORMATTED
~~~roc
{
	foo : 
	
	foo = True

	42
}
~~~
# CANONICALIZE
~~~clojure
(e-block @1.1-6.2
	(s-type-anno @2.5-2.32 (name "foo")
		(ty-malformed @2.26-2.32))
	(s-let @3.5-3.15
		(p-assign @3.5-3.8 (ident "foo"))
		(e-tag @3.11-3.15 (name "True")))
	(e-int @5.5-5.7 (value "42")))
~~~
# TYPES
~~~clojure
(expr @1.1-6.2 (type "Num(*)"))
~~~
