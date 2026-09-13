# META
~~~ini
description=Fold with closure should remain singleline
type=snippet
~~~
# SOURCE
~~~roc
sumResult = fold([1, 2, 3, 4], 0, |acc, x| acc + x)
~~~
# EXPECTED
NAME NOT IN SCOPE - fold_closure.md:1:13:1:17
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Name Not In Scope")
		(region (start 1 13) (end 1 17))
		(headline
			(reflow "Nothing is named ")
			(annotated symbol-unqualified "fold")
			(reflow " in this scope."))
		(document
			(reflow "Is it misspelled, or is there an import missing?")
			(line-break)
			(line-break)
			(source-region (file "fold_closure.md") (start 1 13) (end 1 17) (annotation error) (line-text "sumResult = fold([1, 2, 3, 4], 0, |acc, x| acc + x)")))))
~~~
# TOKENS
~~~zig
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,OpenSquare,Int,Comma,Int,Comma,Int,Comma,Int,CloseSquare,Comma,Int,Comma,OpBar,LowerIdent,Comma,LowerIdent,OpBar,LowerIdent,OpPlus,LowerIdent,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-decl
			(p-ident (raw "sumResult"))
			(e-apply
				(e-ident (raw "fold"))
				(e-list
					(e-int (raw "1"))
					(e-int (raw "2"))
					(e-int (raw "3"))
					(e-int (raw "4")))
				(e-int (raw "0"))
				(e-lambda
					(args
						(p-ident (raw "acc"))
						(p-ident (raw "x")))
					(e-binop (op "+")
						(e-ident (raw "acc"))
						(e-ident (raw "x"))))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "sumResult"))
		(e-runtime-error (tag "erroneous_value_expr"))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Error")))
	(expressions
		(expr (type "Error"))))
~~~
