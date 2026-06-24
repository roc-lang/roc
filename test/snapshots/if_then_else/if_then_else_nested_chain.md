# META
~~~ini
description=Nested if-then-else chain demonstrating flattening
type=snippet
~~~
# SOURCE
~~~roc
checkNumber = |num| {
	if num < 0 {
		"negative"
	} else if num == 0 {
		"zero"
	} else if num > 100 {
		"large"
	} else {
		"positive"
	}
}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenCurly,
KwIf,LowerIdent,OpLessThan,Int,OpenCurly,
StringStart,StringPart,StringEnd,
CloseCurly,KwElse,KwIf,LowerIdent,OpEquals,Int,OpenCurly,
StringStart,StringPart,StringEnd,
CloseCurly,KwElse,KwIf,LowerIdent,OpGreaterThan,Int,OpenCurly,
StringStart,StringPart,StringEnd,
CloseCurly,KwElse,OpenCurly,
StringStart,StringPart,StringEnd,
CloseCurly,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-decl
			(p-ident (raw "checkNumber"))
			(e-lambda
				(args
					(p-ident (raw "num")))
				(e-block
					(statements
						(e-if-then-else
							(e-binop (op "<")
								(e-ident (raw "num"))
								(e-int (raw "0")))
							(e-block
								(statements
									(e-string
										(e-string-part (raw "negative")))))
							(e-if-then-else
								(e-binop (op "==")
									(e-ident (raw "num"))
									(e-int (raw "0")))
								(e-block
									(statements
										(e-string
											(e-string-part (raw "zero")))))
								(e-if-then-else
									(e-binop (op ">")
										(e-ident (raw "num"))
										(e-int (raw "100")))
									(e-block
										(statements
											(e-string
												(e-string-part (raw "large")))))
									(e-block
										(statements
											(e-string
												(e-string-part (raw "positive"))))))))))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "checkNumber"))
		(e-lambda
			(args
				(p-assign (ident "num")))
			(e-block
				(e-if
					(if-branches
						(if-branch
							(e-dispatch-call (method "is_lt") (constraint-fn-var 72)
								(receiver
									(e-lookup-local
										(p-assign (ident "num"))))
								(args
									(e-num (value "0"))))
							(e-block
								(e-string
									(e-literal (string "negative")))))
						(if-branch
							(e-method-eq (negated "false")
								(lhs
									(e-lookup-local
										(p-assign (ident "num"))))
								(rhs
									(e-num (value "0"))))
							(e-block
								(e-string
									(e-literal (string "zero")))))
						(if-branch
							(e-dispatch-call (method "is_gt") (constraint-fn-var 186)
								(receiver
									(e-lookup-local
										(p-assign (ident "num"))))
								(args
									(e-num (value "100"))))
							(e-block
								(e-string
									(e-literal (string "large"))))))
					(if-else
						(e-block
							(e-string
								(e-literal (string "positive"))))))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "a -> b where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)]), a.is_eq : a, a -> Bool, a.is_gt : a, a -> Bool, a.is_lt : a, a -> Bool, b.from_quote : Str -> Try(b, [BadQuotedBytes(Str)])]")))
	(expressions
		(expr (type "a -> b where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)]), a.is_eq : a, a -> Bool, a.is_gt : a, a -> Bool, a.is_lt : a, a -> Bool, b.from_quote : Str -> Try(b, [BadQuotedBytes(Str)])]"))))
~~~
