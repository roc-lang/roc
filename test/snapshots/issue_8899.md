# META
~~~ini
description=Issue 8899 - lambda with List.append and List.last
type=expr
~~~
# SOURCE
~~~roc
{
    sum_with_last = |l| {
        var $total = 0i64
        var $acc = [0i64]
        for e in l {
            $acc = List.append($acc, e)
            $total = match List.last($acc) { Ok(last) => $total + last, Err(_) => $total }
        }
        $total
    }
    sum_with_last([10i64, 20i64, 30i64])
}
~~~
# EXPECTED
DEPRECATED NUMBER SUFFIX - issue_8899.md:3:22:3:26
DEPRECATED NUMBER SUFFIX - issue_8899.md:4:21:4:25
DEPRECATED NUMBER SUFFIX - issue_8899.md:11:20:11:25
DEPRECATED NUMBER SUFFIX - issue_8899.md:11:27:11:32
DEPRECATED NUMBER SUFFIX - issue_8899.md:11:34:11:39
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Deprecated Number Suffix")
		(region (start 3 22) (end 3 26))
		(headline
			(reflow "This number literal uses an old suffix syntax."))
		(document
			(text "The suffix ")
			(annotated code "i64")
			(text " is deprecated. Write ")
			(annotated code "0.I64")
			(text " instead.")
			(line-break)
			(line-break)
			(source-region (file "issue_8899.md") (start 3 22) (end 3 26) (annotation error) (line-text "        var $total = 0i64"))))
	(report
		(severity runtime_error)
		(title "Deprecated Number Suffix")
		(region (start 4 21) (end 4 25))
		(headline
			(reflow "This number literal uses an old suffix syntax."))
		(document
			(text "The suffix ")
			(annotated code "i64")
			(text " is deprecated. Write ")
			(annotated code "0.I64")
			(text " instead.")
			(line-break)
			(line-break)
			(source-region (file "issue_8899.md") (start 4 21) (end 4 25) (annotation error) (line-text "        var $acc = [0i64]"))))
	(report
		(severity runtime_error)
		(title "Deprecated Number Suffix")
		(region (start 11 20) (end 11 25))
		(headline
			(reflow "This number literal uses an old suffix syntax."))
		(document
			(text "The suffix ")
			(annotated code "i64")
			(text " is deprecated. Write ")
			(annotated code "10.I64")
			(text " instead.")
			(line-break)
			(line-break)
			(source-region (file "issue_8899.md") (start 11 20) (end 11 25) (annotation error) (line-text "    sum_with_last([10i64, 20i64, 30i64])"))))
	(report
		(severity runtime_error)
		(title "Deprecated Number Suffix")
		(region (start 11 27) (end 11 32))
		(headline
			(reflow "This number literal uses an old suffix syntax."))
		(document
			(text "The suffix ")
			(annotated code "i64")
			(text " is deprecated. Write ")
			(annotated code "20.I64")
			(text " instead.")
			(line-break)
			(line-break)
			(source-region (file "issue_8899.md") (start 11 27) (end 11 32) (annotation error) (line-text "    sum_with_last([10i64, 20i64, 30i64])"))))
	(report
		(severity runtime_error)
		(title "Deprecated Number Suffix")
		(region (start 11 34) (end 11 39))
		(headline
			(reflow "This number literal uses an old suffix syntax."))
		(document
			(text "The suffix ")
			(annotated code "i64")
			(text " is deprecated. Write ")
			(annotated code "30.I64")
			(text " instead.")
			(line-break)
			(line-break)
			(source-region (file "issue_8899.md") (start 11 34) (end 11 39) (annotation error) (line-text "    sum_with_last([10i64, 20i64, 30i64])")))))
~~~
# TOKENS
~~~zig
OpenCurly,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenCurly,
KwVar,LowerIdent,OpAssign,Int,
KwVar,LowerIdent,OpAssign,OpenSquare,Int,CloseSquare,
KwFor,LowerIdent,KwIn,LowerIdent,OpenCurly,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,Comma,LowerIdent,CloseRound,
LowerIdent,OpAssign,KwMatch,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpenCurly,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpFatArrow,LowerIdent,OpPlus,LowerIdent,Comma,UpperIdent,NoSpaceOpenRound,Underscore,CloseRound,OpFatArrow,LowerIdent,CloseCurly,
CloseCurly,
LowerIdent,
CloseCurly,
LowerIdent,NoSpaceOpenRound,OpenSquare,Int,Comma,Int,Comma,Int,CloseSquare,CloseRound,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-block
	(statements
		(s-decl
			(p-ident (raw "sum_with_last"))
			(e-lambda
				(args
					(p-ident (raw "l")))
				(e-block
					(statements
						(s-var (name "$total")
							(e-typed-int (raw "0i64") (type "I64")))
						(s-var (name "$acc")
							(e-list
								(e-typed-int (raw "0i64") (type "I64"))))
						(s-for
							(p-ident (raw "e"))
							(e-ident (raw "l"))
							(e-block
								(statements
									(s-decl
										(p-ident (raw "$acc"))
										(e-apply
											(e-ident (raw "List.append"))
											(e-ident (raw "$acc"))
											(e-ident (raw "e"))))
									(s-decl
										(p-ident (raw "$total"))
										(e-match
											(e-apply
												(e-ident (raw "List.last"))
												(e-ident (raw "$acc")))
											(branches
												(branch
													(p-tag (raw "Ok")
														(p-ident (raw "last")))
													(e-binop (op "+")
														(e-ident (raw "$total"))
														(e-ident (raw "last"))))
												(branch
													(p-tag (raw "Err")
														(p-underscore))
													(e-ident (raw "$total")))))))))
						(e-ident (raw "$total"))))))
		(e-apply
			(e-ident (raw "sum_with_last"))
			(e-list
				(e-typed-int (raw "10i64") (type "I64"))
				(e-typed-int (raw "20i64") (type "I64"))
				(e-typed-int (raw "30i64") (type "I64"))))))
~~~
# FORMATTED
~~~roc
{
	sum_with_last = |l| {
		var $total = 0i64.I64
		var $acc = [0i64.I64]
		for e in l {
			$acc = List.append($acc, e)
			$total = match List.last($acc) {
				Ok(last) => $total + last
				Err(_) => $total
			}
		}
		$total
	}
	sum_with_last([10i64.I64, 20i64.I64, 30i64.I64])
}
~~~
# CANONICALIZE
~~~clojure
(e-block
	(s-let
		(p-assign (ident "sum_with_last"))
		(e-lambda
			(args
				(p-assign (ident "l")))
			(e-block
				(s-var
					(p-assign (ident "$total"))
					(e-typed-int (value "0") (type "I64")))
				(s-var
					(p-assign (ident "$acc"))
					(e-list
						(elems
							(e-typed-int (value "0") (type "I64")))))
				(s-for
					(p-assign (ident "e"))
					(e-lookup-local
						(p-assign (ident "l")))
					(e-block
						(s-reassign
							(p-assign (ident "$acc"))
							(e-call (constraint-fn-var 305)
								(e-lookup-external
									(builtin))
								(e-lookup-local
									(p-assign (ident "$acc")))
								(e-lookup-local
									(p-assign (ident "e")))))
						(s-reassign
							(p-assign (ident "$total"))
							(e-match
								(match
									(cond
										(e-call (constraint-fn-var 318)
											(e-lookup-external
												(builtin))
											(e-lookup-local
												(p-assign (ident "$acc")))))
									(branches
										(branch
											(patterns
												(pattern (degenerate false)
													(p-applied-tag)))
											(value
												(e-dispatch-call (method "plus") (constraint-fn-var 323)
													(receiver
														(e-lookup-local
															(p-assign (ident "$total"))))
													(args
														(e-lookup-local
															(p-assign (ident "last")))))))
										(branch
											(patterns
												(pattern (degenerate false)
													(p-applied-tag)))
											(value
												(e-lookup-local
													(p-assign (ident "$total")))))))))
						(e-empty_record)))
				(e-lookup-local
					(p-assign (ident "$total"))))))
	(e-call (constraint-fn-var 362)
		(e-lookup-local
			(p-assign (ident "sum_with_last")))
		(e-list
			(elems
				(e-typed-int (value "10") (type "I64"))
				(e-typed-int (value "20") (type "I64"))
				(e-typed-int (value "30") (type "I64"))))))
~~~
# TYPES
~~~clojure
(expr (type "I64"))
~~~
