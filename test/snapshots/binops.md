# META
~~~ini
description=Binops collection
type=expr
~~~
# SOURCE
~~~roc
(
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
    Bool.True and Bool.False,
    Bool.False or Bool.True,
    None ?? 0,
)
~~~
# EXPECTED
UNDECLARED TYPE - binops.md:14:5:14:9
UNDECLARED TYPE - binops.md:14:19:14:23
UNDECLARED TYPE - binops.md:15:5:15:9
UNDECLARED TYPE - binops.md:15:19:15:23
# PROBLEMS
**UNDECLARED TYPE**
The type _Bool_ is not declared in this scope.

This type is referenced here:
**binops.md:14:5:14:9:**
```roc
    Bool.True and Bool.False,
```
    ^^^^


**UNDECLARED TYPE**
The type _Bool_ is not declared in this scope.

This type is referenced here:
**binops.md:14:19:14:23:**
```roc
    Bool.True and Bool.False,
```
                  ^^^^


**UNDECLARED TYPE**
The type _Bool_ is not declared in this scope.

This type is referenced here:
**binops.md:15:5:15:9:**
```roc
    Bool.False or Bool.True,
```
    ^^^^


**UNDECLARED TYPE**
The type _Bool_ is not declared in this scope.

This type is referenced here:
**binops.md:15:19:15:23:**
```roc
    Bool.False or Bool.True,
```
                  ^^^^


# TOKENS
~~~zig
OpenRound,
Int,OpPlus,Int,Comma,
Int,OpBinaryMinus,Int,Comma,
Int,OpStar,Int,Comma,
Int,OpSlash,Int,Comma,
Int,OpPercent,Int,Comma,
Int,OpLessThan,Int,Comma,
Int,OpGreaterThan,Int,Comma,
Int,OpLessThanOrEq,Int,Comma,
Int,OpGreaterThanOrEq,Int,Comma,
Int,OpEquals,Int,Comma,
Int,OpNotEquals,Int,Comma,
Int,OpDoubleSlash,Int,Comma,
UpperIdent,NoSpaceDotUpperIdent,OpAnd,UpperIdent,NoSpaceDotUpperIdent,Comma,
UpperIdent,NoSpaceDotUpperIdent,OpOr,UpperIdent,NoSpaceDotUpperIdent,Comma,
UpperIdent,OpDoubleQuestion,Int,Comma,
CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-tuple
	(e-binop (op "+")
		(e-int (raw "4"))
		(e-int (raw "2")))
	(e-binop (op "-")
		(e-int (raw "4"))
		(e-int (raw "2")))
	(e-binop (op "*")
		(e-int (raw "4"))
		(e-int (raw "2")))
	(e-binop (op "/")
		(e-int (raw "4"))
		(e-int (raw "2")))
	(e-binop (op "%")
		(e-int (raw "4"))
		(e-int (raw "2")))
	(e-binop (op "<")
		(e-int (raw "4"))
		(e-int (raw "2")))
	(e-binop (op ">")
		(e-int (raw "4"))
		(e-int (raw "2")))
	(e-binop (op "<=")
		(e-int (raw "4"))
		(e-int (raw "2")))
	(e-binop (op ">=")
		(e-int (raw "4"))
		(e-int (raw "2")))
	(e-binop (op "==")
		(e-int (raw "4"))
		(e-int (raw "2")))
	(e-binop (op "!=")
		(e-int (raw "4"))
		(e-int (raw "2")))
	(e-binop (op "//")
		(e-int (raw "4"))
		(e-int (raw "2")))
	(e-binop (op "and")
		(e-tag (raw "Bool.True"))
		(e-tag (raw "Bool.False")))
	(e-binop (op "or")
		(e-tag (raw "Bool.False"))
		(e-tag (raw "Bool.True")))
	(e-binop (op "??")
		(e-tag (raw "None"))
		(e-int (raw "0"))))
~~~
# FORMATTED
~~~roc
(
	4 + 2,
	4 - 2,
	4 * 2,
	4 / 2,
	4 % 2,
	4 < 2,
	4 > 2,
	4 <= 2,
	4 >= 2,
	4 == 2,
	4 != 2,
	4 // 2,
	Bool.True and Bool.False,
	Bool.False or Bool.True,
	None ?? 0,
)
~~~
# CANONICALIZE
~~~clojure
(e-tuple
	(elems
		(e-binop (op "add")
			(e-num (value "4"))
			(e-num (value "2")))
		(e-binop (op "sub")
			(e-num (value "4"))
			(e-num (value "2")))
		(e-binop (op "mul")
			(e-num (value "4"))
			(e-num (value "2")))
		(e-binop (op "div")
			(e-num (value "4"))
			(e-num (value "2")))
		(e-binop (op "rem")
			(e-num (value "4"))
			(e-num (value "2")))
		(e-binop (op "lt")
			(e-num (value "4"))
			(e-num (value "2")))
		(e-binop (op "gt")
			(e-num (value "4"))
			(e-num (value "2")))
		(e-binop (op "le")
			(e-num (value "4"))
			(e-num (value "2")))
		(e-binop (op "ge")
			(e-num (value "4"))
			(e-num (value "2")))
		(e-binop (op "eq")
			(e-num (value "4"))
			(e-num (value "2")))
		(e-binop (op "ne")
			(e-num (value "4"))
			(e-num (value "2")))
		(e-binop (op "div_trunc")
			(e-num (value "4"))
			(e-num (value "2")))
		(e-binop (op "and")
			(e-runtime-error (tag "undeclared_type"))
			(e-runtime-error (tag "undeclared_type")))
		(e-binop (op "or")
			(e-runtime-error (tag "undeclared_type"))
			(e-runtime-error (tag "undeclared_type")))
		(e-binop (op "null_coalesce")
			(e-tag (name "None"))
			(e-num (value "0")))))
~~~
# TYPES
~~~clojure
(expr (type "(Num(_size), Num(_size2), Num(_size3), Num(_size4), Num(_size5), Num(_size6), Num(_size7), Num(_size8), Num(_size9), Num(_size10), Num(_size11), Num(_size12), Error, Error, _field)"))
~~~
