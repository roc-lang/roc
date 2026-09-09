# META
~~~ini
description=Basic variable scoping behavior
type=snippet
~~~
# SOURCE
~~~roc
# Top-level variables
x = 5
y = 10

# Function that shadows outer variable
outerFunc = |_| {
    x = 20  # Should shadow top-level x
    innerResult = {
        # Block scope
        z = x + y  # x should resolve to 20, y to 10
        z + 1
    }
    innerResult
}
~~~
# EXPECTED
DUPLICATE DEFINITION - can_basic_scoping.md:7:5:7:6
# PROBLEMS
~~~clojure
(reports
	(report
		(severity warning)
		(title "Duplicate Definition")
		(region (start 7 5) (end 7 6))
		(headline
			(reflow "The name ")
			(annotated symbol-unqualified "x")
			(reflow " is being redeclared here:"))
		(document
			(source-region (file "can_basic_scoping.md") (start 7 5) (end 7 6) (annotation error) (line-text "    x = 20  # Should shadow top-level x"))
			(line-break)
			(reflow "In this scope, ")
			(annotated symbol-unqualified "x")
			(reflow " was already defined in ")
			(source-location
				(file "can_basic_scoping.md")
				(line 2)
				(column 1))
			(reflow ":")
			(line-break)
			(source-region (file "can_basic_scoping.md") (start 2 1) (end 2 2) (annotation dim) (line-text "x = 5")))))
~~~
# TOKENS
~~~zig
LowerIdent,OpAssign,Int,
LowerIdent,OpAssign,Int,
LowerIdent,OpAssign,OpBar,Underscore,OpBar,OpenCurly,
LowerIdent,OpAssign,Int,
LowerIdent,OpAssign,OpenCurly,
LowerIdent,OpAssign,LowerIdent,OpPlus,LowerIdent,
LowerIdent,OpPlus,Int,
CloseCurly,
LowerIdent,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-decl
			(p-ident (raw "x"))
			(e-int (raw "5")))
		(s-decl
			(p-ident (raw "y"))
			(e-int (raw "10")))
		(s-decl
			(p-ident (raw "outerFunc"))
			(e-lambda
				(args
					(p-underscore))
				(e-block
					(statements
						(s-decl
							(p-ident (raw "x"))
							(e-int (raw "20")))
						(s-decl
							(p-ident (raw "innerResult"))
							(e-block
								(statements
									(s-decl
										(p-ident (raw "z"))
										(e-binop (op "+")
											(e-ident (raw "x"))
											(e-ident (raw "y"))))
									(e-binop (op "+")
										(e-ident (raw "z"))
										(e-int (raw "1"))))))
						(e-ident (raw "innerResult"))))))))
~~~
# FORMATTED
~~~roc
# Top-level variables
x = 5

y = 10

# Function that shadows outer variable
outerFunc = |_| {
	x = 20 # Should shadow top-level x
	innerResult = {
		# Block scope
		z = x + y # x should resolve to 20, y to 10
		z + 1
	}
	innerResult
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "x"))
		(e-num (value "5")))
	(d-let
		(p-assign (ident "y"))
		(e-num (value "10")))
	(d-let
		(p-assign (ident "outerFunc"))
		(e-lambda
			(args
				(p-underscore))
			(e-block
				(s-let
					(p-assign (ident "x"))
					(e-num (value "20")))
				(s-let
					(p-assign (ident "innerResult"))
					(e-block
						(s-let
							(p-assign (ident "z"))
							(e-dispatch-call (method "plus") (constraint-fn-var 248)
								(receiver
									(e-lookup-local
										(p-assign (ident "x"))))
								(args
									(e-lookup-local
										(p-assign (ident "y"))))))
						(e-dispatch-call (method "plus") (constraint-fn-var 257)
							(receiver
								(e-lookup-local
									(p-assign (ident "z"))))
							(args
								(e-num (value "1"))))))
				(e-lookup-local
					(p-assign (ident "innerResult")))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Dec"))
		(patt (type "Dec"))
		(patt (type "_arg -> a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)]), a.plus : a, Dec -> a]")))
	(expressions
		(expr (type "Dec"))
		(expr (type "Dec"))
		(expr (type "_arg -> a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)]), a.plus : a, Dec -> a]"))))
~~~
