# META
~~~ini
description=for! loops pull from a Stream with the effectful next!, so they require an effectful function
type=snippet
~~~
# SOURCE
~~~roc
sum_stream! : Stream(U64) => U64
sum_stream! = |s| {
    var $total = 0
    for! n in s {
        $total = $total + n
    }
    $total
}

print_all! : Stream(Str) => {}
print_all! = |s| for! word in s { dbg word }

pure_sum : Stream(U64) -> U64
pure_sum = |s| {
    var $total = 0
    for! n in s {
        $total = $total + n
    }
    $total
}

plain_for : Stream(U64) -> U64
plain_for = |s| {
    var $total = 0
    for n in s {
        $total = $total + n
    }
    $total
}

top_level = {
    var $total = 0
    for! n in [1.U64, 2].iter() {
        $total = $total + n
    }
    $total
}
~~~
# EXPECTED
TYPE MISMATCH - for_bang_stream.md:14:12:14:15
EFFECTFUL FUNCTION NAME - for_bang_stream.md:14:1:14:9
MISSING METHOD - for_bang_stream.md:25:14:25:15
EFFECTFUL TOP LEVEL VALUE - for_bang_stream.md:31:13:37:2
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Type Mismatch")
		(region (start 14 12) (end 14 15))
		(headline
			(reflow "This expression is used in an unexpected way."))
		(document
			(source-region (file "for_bang_stream.md") (start 14 12) (end 14 15) (annotation error) (line-text "pure_sum = |s| {"))
			(line-break)
			(reflow "It has the type:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "Stream(U64) => U64")
			(annotation-end)
			(line-break)
			(line-break)
			(reflow "But the annotation says it should be:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "Stream(U64) -> U64")
			(annotation-end)
			(line-break)
			(line-break)
			(annotated emphasis "Hint:")
			(reflow " ")
			(reflow "This function is effectful, but a pure function is expected.")))
	(report
		(severity warning)
		(title "Effectful Function Name")
		(region (start 14 1) (end 14 9))
		(headline
			(reflow "This function performs an effect, so its name must end in `!`."))
		(document
			(source-region (file "for_bang_stream.md") (start 14 1) (end 14 9) (annotation warning) (line-text "pure_sum = |s| {"))
			(line-break)
			(line-break)
			(reflow "Add a trailing")
			(reflow " ")
			(annotated code "!")
			(reflow " ")
			(reflow "to this function name.")))
	(report
		(severity runtime_error)
		(title "Missing Method")
		(region (start 25 14) (end 25 15))
		(headline
			(reflow "This")
			(reflow " ")
			(annotated code "iter")
			(reflow " ")
			(reflow "method is being called on a value whose type doesn't have that method."))
		(document
			(source-region (file "for_bang_stream.md") (start 25 14) (end 25 15) (annotation error) (line-text "    for n in s {"))
			(line-break)
			(reflow "The value's type, which does not have a method named ")
			(annotated code "iter")
			(reflow ",")
			(reflow " ")
			(reflow "is:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "Stream(U64)")
			(annotation-end)
			(line-break)
			(line-break)
			(annotated emphasis "Hint:")
			(reflow " ")
			(reflow "A")
			(reflow " ")
			(annotated code "for")
			(reflow " ")
			(reflow "loop can only go through pure iterators. To loop over a")
			(reflow " ")
			(annotated code "Stream")
			(reflow ", use")
			(reflow " ")
			(annotated code "for!")
			(reflow " ")
			(reflow "instead, which pulls each item with the effectful")
			(reflow " ")
			(annotated code "next!")
			(reflow " ")
			(reflow "and so can only be used in an effectful function.")))
	(report
		(severity runtime_error)
		(title "Effectful Top Level Value")
		(region (start 31 13) (end 37 2))
		(headline
			(reflow "This top-level definition performs an effect while initializing."))
		(document
			(source-region (file "for_bang_stream.md") (start 31 13) (end 37 2) (annotation error) (line-text "top_level = {\n    var $total = 0\n    for! n in [1.U64, 2].iter() {\n        $total = $total + n\n    }\n    $total\n}"))
			(line-break)
			(line-break)
			(reflow "Move the effect into a function body so it runs when the function is called."))))
~~~
# TOKENS
~~~zig
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,OpFatArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenCurly,
KwVar,LowerIdent,OpAssign,Int,
KwForBang,LowerIdent,KwIn,LowerIdent,OpenCurly,
LowerIdent,OpAssign,LowerIdent,OpPlus,LowerIdent,
CloseCurly,
LowerIdent,
CloseCurly,
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,OpFatArrow,OpenCurly,CloseCurly,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,KwForBang,LowerIdent,KwIn,LowerIdent,OpenCurly,KwDbg,LowerIdent,CloseCurly,
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenCurly,
KwVar,LowerIdent,OpAssign,Int,
KwForBang,LowerIdent,KwIn,LowerIdent,OpenCurly,
LowerIdent,OpAssign,LowerIdent,OpPlus,LowerIdent,
CloseCurly,
LowerIdent,
CloseCurly,
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenCurly,
KwVar,LowerIdent,OpAssign,Int,
KwFor,LowerIdent,KwIn,LowerIdent,OpenCurly,
LowerIdent,OpAssign,LowerIdent,OpPlus,LowerIdent,
CloseCurly,
LowerIdent,
CloseCurly,
LowerIdent,OpAssign,OpenCurly,
KwVar,LowerIdent,OpAssign,Int,
KwForBang,LowerIdent,KwIn,OpenSquare,Int,NoSpaceDotUpperIdent,Comma,Int,CloseSquare,NoSpaceDotLowerIdent,NoSpaceOpenRound,CloseRound,OpenCurly,
LowerIdent,OpAssign,LowerIdent,OpPlus,LowerIdent,
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
		(s-type-anno (name "sum_stream!")
			(ty-fn
				(ty-apply
					(ty (name "Stream"))
					(ty (name "U64")))
				(ty (name "U64"))))
		(s-decl
			(p-ident (raw "sum_stream!"))
			(e-lambda
				(args
					(p-ident (raw "s")))
				(e-block
					(statements
						(s-var (name "$total")
							(e-int (raw "0")))
						(s-for-bang
							(p-ident (raw "n"))
							(e-ident (raw "s"))
							(e-block
								(statements
									(s-decl
										(p-ident (raw "$total"))
										(e-binop (op "+")
											(e-ident (raw "$total"))
											(e-ident (raw "n")))))))
						(e-ident (raw "$total"))))))
		(s-type-anno (name "print_all!")
			(ty-fn
				(ty-apply
					(ty (name "Stream"))
					(ty (name "Str")))
				(ty-record)))
		(s-decl
			(p-ident (raw "print_all!"))
			(e-lambda
				(args
					(p-ident (raw "s")))
				(e-for-bang
					(p-ident (raw "word"))
					(e-ident (raw "s"))
					(e-block
						(statements
							(s-dbg
								(e-ident (raw "word"))))))))
		(s-type-anno (name "pure_sum")
			(ty-fn
				(ty-apply
					(ty (name "Stream"))
					(ty (name "U64")))
				(ty (name "U64"))))
		(s-decl
			(p-ident (raw "pure_sum"))
			(e-lambda
				(args
					(p-ident (raw "s")))
				(e-block
					(statements
						(s-var (name "$total")
							(e-int (raw "0")))
						(s-for-bang
							(p-ident (raw "n"))
							(e-ident (raw "s"))
							(e-block
								(statements
									(s-decl
										(p-ident (raw "$total"))
										(e-binop (op "+")
											(e-ident (raw "$total"))
											(e-ident (raw "n")))))))
						(e-ident (raw "$total"))))))
		(s-type-anno (name "plain_for")
			(ty-fn
				(ty-apply
					(ty (name "Stream"))
					(ty (name "U64")))
				(ty (name "U64"))))
		(s-decl
			(p-ident (raw "plain_for"))
			(e-lambda
				(args
					(p-ident (raw "s")))
				(e-block
					(statements
						(s-var (name "$total")
							(e-int (raw "0")))
						(s-for
							(p-ident (raw "n"))
							(e-ident (raw "s"))
							(e-block
								(statements
									(s-decl
										(p-ident (raw "$total"))
										(e-binop (op "+")
											(e-ident (raw "$total"))
											(e-ident (raw "n")))))))
						(e-ident (raw "$total"))))))
		(s-decl
			(p-ident (raw "top_level"))
			(e-block
				(statements
					(s-var (name "$total")
						(e-int (raw "0")))
					(s-for-bang
						(p-ident (raw "n"))
						(e-method-call (method ".iter")
							(receiver
								(e-list
									(e-typed-int (raw "1") (type "U64"))
									(e-int (raw "2"))))
							(args))
						(e-block
							(statements
								(s-decl
									(p-ident (raw "$total"))
									(e-binop (op "+")
										(e-ident (raw "$total"))
										(e-ident (raw "n")))))))
					(e-ident (raw "$total")))))))
~~~
# FORMATTED
~~~roc
sum_stream! : Stream(U64) => U64
sum_stream! = |s| {
	var $total = 0
	for! n in s {
		$total = $total + n
	}
	$total
}

print_all! : Stream(Str) => {}
print_all! = |s| for! word in s {
	dbg word
}

pure_sum : Stream(U64) -> U64
pure_sum = |s| {
	var $total = 0
	for! n in s {
		$total = $total + n
	}
	$total
}

plain_for : Stream(U64) -> U64
plain_for = |s| {
	var $total = 0
	for n in s {
		$total = $total + n
	}
	$total
}

top_level = {
	var $total = 0
	for! n in [1.U64, 2].iter() {
		$total = $total + n
	}
	$total
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "sum_stream!"))
		(e-lambda
			(args
				(p-assign (ident "s")))
			(e-block
				(s-var
					(p-var-assign (ident "$total"))
					(e-num (value "0")))
				(s-for-bang
					(p-assign (ident "n"))
					(e-lookup-local
						(p-assign (ident "s")))
					(e-block
						(s-reassign
							(p-var-assign (ident "$total"))
							(e-dispatch-call (method "plus") (constraint-fn-var 388)
								(receiver
									(e-lookup-local
										(p-var-assign (ident "$total"))))
								(args
									(e-lookup-local
										(p-assign (ident "n"))))))
						(e-empty_record)))
				(e-lookup-local
					(p-var-assign (ident "$total")))))
		(annotation
			(ty-fn (effectful true)
				(ty-apply (name "Stream") (builtin)
					(ty-lookup (name "U64") (builtin)))
				(ty-lookup (name "U64") (builtin)))))
	(d-let
		(p-assign (ident "print_all!"))
		(e-lambda
			(args
				(p-assign (ident "s")))
			(e-for-bang
				(p-assign (ident "word"))
				(e-lookup-local
					(p-assign (ident "s")))
				(e-block
					(e-dbg
						(e-lookup-local
							(p-assign (ident "word")))))))
		(annotation
			(ty-fn (effectful true)
				(ty-apply (name "Stream") (builtin)
					(ty-lookup (name "Str") (builtin)))
				(ty-record))))
	(d-let
		(p-assign (ident "pure_sum"))
		(e-runtime-error (tag "erroneous_value_expr"))
		(annotation
			(ty-fn (effectful false)
				(ty-apply (name "Stream") (builtin)
					(ty-lookup (name "U64") (builtin)))
				(ty-lookup (name "U64") (builtin)))))
	(d-let
		(p-assign (ident "plain_for"))
		(e-lambda
			(args
				(p-assign (ident "s")))
			(e-block
				(s-var
					(p-var-assign (ident "$total"))
					(e-num (value "0")))
				(s-for
					(p-assign (ident "n"))
					(e-lookup-local
						(p-assign (ident "s")))
					(e-block
						(s-reassign
							(p-var-assign (ident "$total"))
							(e-dispatch-call (method "plus") (constraint-fn-var 527)
								(receiver
									(e-lookup-local
										(p-var-assign (ident "$total"))))
								(args
									(e-lookup-local
										(p-assign (ident "n"))))))
						(e-empty_record)))
				(e-lookup-local
					(p-var-assign (ident "$total")))))
		(annotation
			(ty-fn (effectful false)
				(ty-apply (name "Stream") (builtin)
					(ty-lookup (name "U64") (builtin)))
				(ty-lookup (name "U64") (builtin)))))
	(d-let
		(p-assign (ident "top_level"))
		(e-runtime-error (tag "erroneous_value_expr"))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Stream(U64) => U64"))
		(patt (type "Stream(Str) => {}"))
		(patt (type "Stream(U64) -> U64"))
		(patt (type "Stream(U64) -> U64"))
		(patt (type "Error")))
	(expressions
		(expr (type "Stream(U64) => U64"))
		(expr (type "Stream(Str) => {}"))
		(expr (type "Stream(U64) -> U64"))
		(expr (type "Stream(U64) -> U64"))
		(expr (type "Error"))))
~~~
