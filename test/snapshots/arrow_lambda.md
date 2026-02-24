# META
~~~ini
description=Arrow syntax with parenthesized lambda expressions
type=snippet
~~~
# SOURCE
~~~roc
# Basic lambda after arrow
test1 = 10->(|x| x + 1)

# Lambda ignoring argument
test2 = "hello"->(|_| "world")

# Lambda with if expression
test3 = ""->(|s| if s.is_empty() "empty" else "not empty")
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent,OpAssign,Int,OpArrow,NoSpaceOpenRound,OpBar,LowerIdent,OpBar,LowerIdent,OpPlus,Int,CloseRound,
LowerIdent,OpAssign,StringStart,StringPart,StringEnd,OpArrow,NoSpaceOpenRound,OpBar,Underscore,OpBar,StringStart,StringPart,StringEnd,CloseRound,
LowerIdent,OpAssign,StringStart,StringPart,StringEnd,OpArrow,NoSpaceOpenRound,OpBar,LowerIdent,OpBar,KwIf,LowerIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,CloseRound,StringStart,StringPart,StringEnd,KwElse,StringStart,StringPart,StringEnd,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-decl
			(p-ident (raw "test1"))
			(e-local-dispatch
				(e-int (raw "10"))
				(e-lambda
					(args
						(p-ident (raw "x")))
					(e-binop (op "+")
						(e-ident (raw "x"))
						(e-int (raw "1"))))))
		(s-decl
			(p-ident (raw "test2"))
			(e-local-dispatch
				(e-string
					(e-string-part (raw "hello")))
				(e-lambda
					(args
						(p-underscore))
					(e-string
						(e-string-part (raw "world"))))))
		(s-decl
			(p-ident (raw "test3"))
			(e-local-dispatch
				(e-string
					(e-string-part (raw "")))
				(e-lambda
					(args
						(p-ident (raw "s")))
					(e-if-then-else
						(e-field-access
							(e-ident (raw "s"))
							(e-apply
								(e-ident (raw "is_empty"))))
						(e-string
							(e-string-part (raw "empty")))
						(e-string
							(e-string-part (raw "not empty")))))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "test1"))
		(e-call
			(e-lambda
				(args
					(p-assign (ident "x")))
				(e-binop (op "add")
					(e-lookup-local
						(p-assign (ident "x")))
					(e-num (value "1"))))
			(e-num (value "10"))))
	(d-let
		(p-assign (ident "test2"))
		(e-call
			(e-lambda
				(args
					(p-underscore))
				(e-string
					(e-literal (string "world"))))
			(e-string
				(e-literal (string "hello")))))
	(d-let
		(p-assign (ident "test3"))
		(e-call
			(e-lambda
				(args
					(p-assign (ident "s")))
				(e-if
					(if-branches
						(if-branch
							(e-dot-access (field "is_empty")
								(receiver
									(e-lookup-local
										(p-assign (ident "s"))))
								(args))
							(e-string
								(e-literal (string "empty")))))
					(if-else
						(e-string
							(e-literal (string "not empty"))))))
			(e-string
				(e-literal (string ""))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Dec"))
		(patt (type "Str"))
		(patt (type "Str")))
	(expressions
		(expr (type "Dec"))
		(expr (type "Str"))
		(expr (type "Str"))))
~~~
