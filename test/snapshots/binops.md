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
NOT IMPLEMENTED - :0:0:0:0
# PROBLEMS
**NOT IMPLEMENTED**
This feature is not yet implemented: unsupported operator

This error doesn't have a proper diagnostic report yet. Let us know if you want to help improve Roc's error messages!

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
			(e-nominal-external
				(builtin)
				(e-tag (name "True")))
			(e-nominal-external
				(builtin)
				(e-tag (name "False"))))
		(e-binop (op "or")
			(e-nominal-external
				(builtin)
				(e-tag (name "False")))
			(e-nominal-external
				(builtin)
				(e-tag (name "True"))))
		(e-runtime-error (tag "not_implemented"))))
~~~
# TYPES
~~~clojure
(expr (type "(a, b, c, d, e, Bool, Bool, Bool, Bool, Bool, Bool, f, Error, Error, Error) where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)]), b.from_numeral : Numeral -> Try(b, [InvalidNumeral(Str)]), c.from_numeral : Numeral -> Try(c, [InvalidNumeral(Str)]), d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)]), e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)]), f.from_numeral : Numeral -> Try(f, [InvalidNumeral(Str)])]"))
~~~
