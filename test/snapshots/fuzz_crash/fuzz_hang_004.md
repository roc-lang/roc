# META
~~~ini
description=canonicalize hang: recursive slice pattern matcher with partial tokens
type=file
source_escapes=true
~~~
# SOURCE
~~~roc
s={match 0{[]=>[][]=>{{()}{}.70000}}}a=||{}
~~~
# EXPECTED
EMPTY TUPLE NOT ALLOWED - fuzz_hang_004.md:1:24:1:26
TYPE MISMATCH - fuzz_hang_004.md:1:10:1:11
INVALID TUPLE ACCESS - fuzz_hang_004.md:1:27:1:35
REDUNDANT PATTERN - fuzz_hang_004.md:1:4:1:37
NON EXHAUSTIVE MATCH - fuzz_hang_004.md:1:4:1:37
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Empty Tuple Not Allowed")
		(region (start 1 24) (end 1 26))
		(headline
			(reflow "I am part way through parsing this tuple, but it is empty."))
		(document
			(source-region (file "fuzz_hang_004.md") (start 1 24) (end 1 26) (annotation error) (line-text "s={match 0{[]=>[][]=>{{()}{}.70000}}}a=||{}"))
			(line-break)
			(reflow "If you want to represent nothing, try using an empty record: ")
			(annotated code "{}")
			(reflow ".")))
	(report
		(severity runtime_error)
		(title "Type Mismatch")
		(region (start 1 10) (end 1 11))
		(headline
			(reflow "This number is being used where a non-number type is needed."))
		(document
			(source-region (file "fuzz_hang_004.md") (start 1 10) (end 1 11) (annotation error) (line-text "s={match 0{[]=>[][]=>{{()}{}.70000}}}a=||{}"))
			(line-break)
			(reflow "The type was determined to be non-numeric here:")
			(line-break)
			(source-region (file "fuzz_hang_004.md") (start 1 12) (end 1 14) (annotation error) (line-text "s={match 0{[]=>[][]=>{{()}{}.70000}}}a=||{}"))
			(line-break)
			(reflow "Other code expects this to have the type:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "List(_b)")
			(annotation-end)))
	(report
		(severity runtime_error)
		(title "Invalid Tuple Access")
		(region (start 1 27) (end 1 35))
		(headline
			(reflow "This value is not a tuple, so it has no .70000 element."))
		(document
			(source-region (file "fuzz_hang_004.md") (start 1 27) (end 1 35) (annotation error) (line-text "s={match 0{[]=>[][]=>{{()}{}.70000}}}a=||{}"))))
	(report
		(severity warning)
		(title "Redundant Pattern")
		(region (start 1 4) (end 1 37))
		(headline
			(reflow "The")
			(reflow " ")
			(reflow "second")
			(reflow " ")
			(reflow "branch of this")
			(reflow " ")
			(annotated keyword "match")
			(reflow " ")
			(reflow "is redundant."))
		(document
			(source-region (file "fuzz_hang_004.md") (start 1 4) (end 1 37) (annotation error) (line-text "s={match 0{[]=>[][]=>{{()}{}.70000}}}a=||{}"))
			(line-break)
			(reflow "This pattern can never match because earlier patterns already cover all the values it would match.")))
	(report
		(severity runtime_error)
		(title "Non Exhaustive Match")
		(region (start 1 4) (end 1 37))
		(headline
			(reflow "This match expression doesn't cover all possible cases."))
		(document
			(source-region (file "fuzz_hang_004.md") (start 1 4) (end 1 37) (annotation error) (line-text "s={match 0{[]=>[][]=>{{()}{}.70000}}}a=||{}"))
			(line-break)
			(reflow "The value being matched on has type:")
			(line-break)
			(text "        ")
			(annotated type "List(_b)")
			(line-break)
			(line-break)
			(reflow "Missing patterns:")
			(line-break)
			(text "    ")
			(annotation-start code-block)
			(indent 1)
			(text "[_, ..]")
			(annotation-end)
			(line-break)
			(line-break)
			(reflow "Hint: Add branches to handle these cases, or use")
			(reflow " ")
			(annotated keyword "_")
			(reflow " ")
			(reflow "to match anything."))))
~~~
# TOKENS
~~~zig
LowerIdent,OpAssign,OpenCurly,KwMatch,Int,OpenCurly,OpenSquare,CloseSquare,OpFatArrow,OpenSquare,CloseSquare,OpenSquare,CloseSquare,OpFatArrow,OpenCurly,OpenCurly,NoSpaceOpenRound,CloseRound,CloseCurly,OpenCurly,CloseCurly,NoSpaceDotInt,CloseCurly,CloseCurly,CloseCurly,LowerIdent,OpAssign,OpBar,OpBar,OpenCurly,CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-decl
			(p-ident (raw "s"))
			(e-block
				(statements
					(e-match
						(e-int (raw "0"))
						(branches
							(branch
								(p-list)
								(e-list))
							(branch
								(p-list)
								(e-block
									(statements
										(e-block
											(statements
												(e-tuple)))
										(e-tuple-access
											(e-record)
											".70000")))))))))
		(s-decl
			(p-ident (raw "a"))
			(e-lambda
				(args)
				(e-record)))))
~~~
# FORMATTED
~~~roc
s = {
	match 0 {
		[] => []
		[] => {
			{
				()
			}
			{}.70000
		}
	}
}

a = || {}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "s"))
		(e-block
			(e-match
				(match
					(cond
						(e-runtime-error (tag "erroneous_value_expr")))
					(branches
						(branch
							(patterns
								(pattern (degenerate false)
									(p-list
										(patterns))))
							(value
								(e-empty_list)))
						(branch
							(patterns
								(pattern (degenerate false)
									(p-list
										(patterns))))
							(value
								(e-runtime-error (tag "erroneous_value_expr")))))))))
	(d-let
		(p-assign (ident "a"))
		(e-lambda
			(args)
			(e-empty_record))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "List(_b)"))
		(patt (type "({}) -> {}")))
	(expressions
		(expr (type "List(_b)"))
		(expr (type "({}) -> {}"))))
~~~
