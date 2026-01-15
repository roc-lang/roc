# META
~~~ini
description=Test basic while loop in block
type=expr
~~~
# SOURCE
~~~roc
{
    var count_ = 0
    while count_ < 10 {
        count_ = count_ + 1
    }
    count_
}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
OpenCurly,
KwVar,LowerIdent,OpAssign,Int,
KwWhile,LowerIdent,OpLessThan,Int,OpenCurly,
LowerIdent,OpAssign,LowerIdent,OpPlus,Int,
CloseCurly,
LowerIdent,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-block
	(statements
		(s-var (name "count_")
			(e-int (raw "0")))
		(s-while
			(e-binop (op "<")
				(e-ident (raw "count_"))
				(e-int (raw "10")))
			(e-block
				(statements
					(s-decl
						(p-ident (raw "count_"))
						(e-binop (op "+")
							(e-ident (raw "count_"))
							(e-int (raw "1")))))))
		(e-ident (raw "count_"))))
~~~
# FORMATTED
~~~roc
{
	var count_ = 0
	while count_ < 10 {
		count_ = count_ + 1
	}
	count_
}
~~~
# CANONICALIZE
~~~clojure
(e-block
	(s-var
		(p-assign (ident "count_"))
		(e-num (value "0")))
	(s-while
		(e-binop (op "lt")
			(e-lookup-local
				(p-assign (ident "count_")))
			(e-num (value "10")))
		(e-block
			(s-reassign
				(p-assign (ident "count_"))
				(e-binop (op "add")
					(e-lookup-local
						(p-assign (ident "count_")))
					(e-num (value "1"))))
			(e-empty_record)))
	(e-lookup-local
		(p-assign (ident "count_"))))
~~~
# TYPES
~~~clojure
(expr (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)]), a.is_lt : a, a -> Bool, a.plus : a, b -> a, b.from_numeral : Numeral -> Try(b, [InvalidNumeral(Str)])]"))
~~~
