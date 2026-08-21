# META
~~~ini
description=Canonicalize crash: duplicate record fields in canonical type key normalization
type=file
~~~
# SOURCE
~~~roc
main! = |_args| {
    dbg Dict.empty().insert({a: 1, b: 2}, 3)
    fn1 = |a,insert({a: 1, a: 2}, 3)nt b||||| a + b Ok({})
}
~~~
# EXPECTED
DUPLICATE RECORD FIELD - fuzz_crash_099.md:3:28:3:29
UNUSED VARIABLE - fuzz_crash_099.md:3:14:3:20
UNUSED VARIABLE - fuzz_crash_099.md:3:37:3:39
UNUSED VARIABLE - fuzz_crash_099.md:3:5:3:8
NON EXHAUSTIVE DESTRUCTURE - fuzz_crash_099.md:3:20:3:37
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Duplicate Record Field")
		(region (start 3 28) (end 3 29))
		(headline
			(reflow "The record field ")
			(annotated record-field "a")
			(reflow " appears more than once in this record."))
		(document
			(source-region (file "fuzz_crash_099.md") (start 3 28) (end 3 29) (annotation error) (line-text "    fn1 = |a,insert({a: 1, a: 2}, 3)nt b||||| a + b Ok({})"))
			(line-break)
			(reflow "The field ")
			(annotated record-field "a")
			(reflow " was first defined in ")
			(source-location
				(file "fuzz_crash_099.md")
				(line 3)
				(column 22))
			(reflow ":")
			(line-break)
			(source-region (file "fuzz_crash_099.md") (start 3 22) (end 3 23) (annotation dim) (line-text "    fn1 = |a,insert({a: 1, a: 2}, 3)nt b||||| a + b Ok({})"))
			(line-break)
			(reflow "Record fields must have unique names. Consider renaming one of these fields or removing the duplicate.")))
	(report
		(severity warning)
		(title "Unused Variable")
		(region (start 3 14) (end 3 20))
		(headline
			(reflow "Variable ")
			(annotated symbol-unqualified "insert")
			(reflow " is defined here and then never used:"))
		(document
			(reflow "If you don't need this variable, prefix it with an underscore like ")
			(annotated symbol-unqualified "_insert")
			(reflow " to suppress this warning.")
			(line-break)
			(source-region (file "fuzz_crash_099.md") (start 3 14) (end 3 20) (annotation error) (line-text "    fn1 = |a,insert({a: 1, a: 2}, 3)nt b||||| a + b Ok({})"))))
	(report
		(severity warning)
		(title "Unused Variable")
		(region (start 3 37) (end 3 39))
		(headline
			(reflow "Variable ")
			(annotated symbol-unqualified "nt")
			(reflow " is defined here and then never used:"))
		(document
			(reflow "If you don't need this variable, prefix it with an underscore like ")
			(annotated symbol-unqualified "_nt")
			(reflow " to suppress this warning.")
			(line-break)
			(source-region (file "fuzz_crash_099.md") (start 3 37) (end 3 39) (annotation error) (line-text "    fn1 = |a,insert({a: 1, a: 2}, 3)nt b||||| a + b Ok({})"))))
	(report
		(severity warning)
		(title "Unused Variable")
		(region (start 3 5) (end 3 8))
		(headline
			(reflow "Variable ")
			(annotated symbol-unqualified "fn1")
			(reflow " is defined here and then never used:"))
		(document
			(reflow "If you don't need this variable, prefix it with an underscore like ")
			(annotated symbol-unqualified "_fn1")
			(reflow " to suppress this warning.")
			(line-break)
			(source-region (file "fuzz_crash_099.md") (start 3 5) (end 3 8) (annotation error) (line-text "    fn1 = |a,insert({a: 1, a: 2}, 3)nt b||||| a + b Ok({})"))))
	(report
		(severity runtime_error)
		(title "Non Exhaustive Destructure")
		(region (start 3 20) (end 3 37))
		(headline
			(reflow "This destructuring pattern doesn't cover all possible cases."))
		(document
			(source-region (file "fuzz_crash_099.md") (start 3 20) (end 3 37) (annotation error) (line-text "    fn1 = |a,insert({a: 1, a: 2}, 3)nt b||||| a + b Ok({})"))
			(line-break)
			(reflow "The value being destructured has type:")
			(line-break)
			(text "        ")
			(annotated type "({ a: c }, d)\n  where [\n    c.from_numeral : Numeral -> Try(c, [InvalidNumeral(Str)]),\n    c.is_eq : c, c -> Bool,\n    d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)]),\n    d.is_eq : d, d -> Bool,\n  ]")
			(line-break)
			(line-break)
			(reflow "Missing patterns:")
			(line-break)
			(text "    ")
			(annotation-start code-block)
			(indent 1)
			(text "({ a: _ }, _)")
			(annotation-end)
			(line-break))))
~~~
# TOKENS
~~~zig
LowerIdent,OpAssign,OpBar,NamedUnderscore,OpBar,OpenCurly,
KwDbg,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,CloseRound,NoSpaceDotLowerIdent,NoSpaceOpenRound,OpenCurly,LowerIdent,OpColon,Int,Comma,LowerIdent,OpColon,Int,CloseCurly,Comma,Int,CloseRound,
LowerIdent,OpAssign,OpBar,LowerIdent,Comma,LowerIdent,NoSpaceOpenRound,OpenCurly,LowerIdent,OpColon,Int,Comma,LowerIdent,OpColon,Int,CloseCurly,Comma,Int,CloseRound,LowerIdent,LowerIdent,OpBar,OpBar,OpBar,OpBar,OpBar,LowerIdent,OpPlus,LowerIdent,UpperIdent,NoSpaceOpenRound,OpenCurly,CloseCurly,CloseRound,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-decl
			(p-ident (raw "main!"))
			(e-lambda
				(args
					(p-ident (raw "_args")))
				(e-block
					(statements
						(s-dbg
							(e-method-call (method ".insert")
								(receiver
									(e-apply
										(e-ident (raw "Dict.empty"))))
								(args
									(e-record
										(field (field "a")
											(e-int (raw "1")))
										(field (field "b")
											(e-int (raw "2"))))
									(e-int (raw "3")))))
						(s-decl
							(p-ident (raw "fn1"))
							(e-lambda
								(args
									(p-ident (raw "a"))
									(p-ident (raw "insert"))
									(p-tuple
										(p-record
											(field (name "a") (rest false)
												(p-int (raw "1")))
											(field (name "a") (rest false)
												(p-int (raw "2"))))
										(p-int (raw "3")))
									(p-ident (raw "nt"))
									(p-ident (raw "b")))
								(e-lambda
									(args)
									(e-lambda
										(args)
										(e-binop (op "+")
											(e-ident (raw "a"))
											(e-ident (raw "b")))))))
						(e-apply
							(e-tag (raw "Ok"))
							(e-record))))))))
~~~
# FORMATTED
~~~roc
main! = |_args| {
	dbg Dict.empty().insert({ a: 1, b: 2 }, 3)
	fn1 = |a, insert, ({ a: 1, a: 2 }, 3), nt, b| || || a + b
	Ok({})
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "echo!"))
		(e-hosted-lambda (symbol "echo!")
			(args
				(p-assign (ident "_echo_arg"))))
		(annotation
			(ty-fn (effectful true)
				(ty-lookup (name "Str") (builtin))
				(ty-record))))
	(d-let
		(p-assign (ident "main!"))
		(e-lambda
			(args
				(p-assign (ident "_args")))
			(e-block
				(s-dbg
					(e-dispatch-call (method "insert") (constraint-fn-var 300)
						(receiver
							(e-call (constraint-fn-var 275)
								(e-lookup-external
									(builtin))))
						(args
							(e-record
								(fields
									(field (name "a")
										(e-num (value "1")))
									(field (name "b")
										(e-num (value "2")))))
							(e-num (value "3")))))
				(s-let
					(p-assign (ident "fn1"))
					(e-lambda
						(args
							(p-assign (ident "a"))
							(p-assign (ident "insert"))
							(p-tuple
								(patterns
									(p-record-destructure
										(destructs
											(record-destruct (label "a") (ident "a")
												(sub-pattern
													(p-num (value "1"))))))
									(p-num (value "3"))))
							(p-assign (ident "nt"))
							(p-assign (ident "b")))
						(e-closure
							(captures
								(capture (ident "a"))
								(capture (ident "b")))
							(e-lambda
								(args)
								(e-closure
									(captures
										(capture (ident "a"))
										(capture (ident "b")))
									(e-lambda
										(args)
										(e-dispatch-call (method "plus") (constraint-fn-var 346)
											(receiver
												(e-lookup-local
													(p-assign (ident "a"))))
											(args
												(e-lookup-local
													(p-assign (ident "b")))))))))))
				(e-tag (name "Ok")
					(args
						(e-empty_record)))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Str => {}"))
		(patt (type "_arg -> [Ok({}), ..]")))
	(expressions
		(expr (type "Str => {}"))
		(expr (type "_arg -> [Ok({}), ..]"))))
~~~
