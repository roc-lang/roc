# META
~~~ini
description=Statements that are not valid in an associated block are reported instead of being silently dropped (issue 10730)
type=file:AssocStmts.roc
~~~
# SOURCE
~~~roc
Counter := [].{
    dbg 5

    crash "boom"

    return 5

    for x in [1, 2] {
        dbg x
    }

    while 1 == 2 {
        dbg 3
    }

    break
}
~~~
# EXPECTED
INVALID STATEMENT - assoc_invalid_statements.md:2:5:2:10
INVALID STATEMENT - assoc_invalid_statements.md:4:5:4:17
INVALID STATEMENT - assoc_invalid_statements.md:6:5:6:13
INVALID STATEMENT - assoc_invalid_statements.md:8:5:10:6
INVALID STATEMENT - assoc_invalid_statements.md:12:5:14:6
INVALID STATEMENT - assoc_invalid_statements.md:16:5:16:10
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Invalid Statement")
		(region (start 2 5) (end 2 10))
		(headline
			(reflow "The statement ")
			(annotated code "dbg")
			(reflow " is not allowed in an associated block."))
		(document
			(reflow "Only associated values, type declarations, and type annotations are allowed in an associated block.")
			(line-break)
			(line-break)
			(source-region (file "assoc_invalid_statements.md") (start 2 5) (end 2 10) (annotation error) (line-text "    dbg 5"))))
	(report
		(severity runtime_error)
		(title "Invalid Statement")
		(region (start 4 5) (end 4 17))
		(headline
			(reflow "The statement ")
			(annotated code "crash")
			(reflow " is not allowed in an associated block."))
		(document
			(reflow "Only associated values, type declarations, and type annotations are allowed in an associated block.")
			(line-break)
			(line-break)
			(source-region (file "assoc_invalid_statements.md") (start 4 5) (end 4 17) (annotation error) (line-text "    crash \"boom\""))))
	(report
		(severity runtime_error)
		(title "Invalid Statement")
		(region (start 6 5) (end 6 13))
		(headline
			(reflow "The statement ")
			(annotated code "return")
			(reflow " is not allowed in an associated block."))
		(document
			(reflow "Only associated values, type declarations, and type annotations are allowed in an associated block.")
			(line-break)
			(line-break)
			(source-region (file "assoc_invalid_statements.md") (start 6 5) (end 6 13) (annotation error) (line-text "    return 5"))))
	(report
		(severity runtime_error)
		(title "Invalid Statement")
		(region (start 8 5) (end 10 6))
		(headline
			(reflow "The statement ")
			(annotated code "for")
			(reflow " is not allowed in an associated block."))
		(document
			(reflow "Only associated values, type declarations, and type annotations are allowed in an associated block.")
			(line-break)
			(line-break)
			(source-region (file "assoc_invalid_statements.md") (start 8 5) (end 10 6) (annotation error) (line-text "    for x in [1, 2] {\n        dbg x\n    }"))))
	(report
		(severity runtime_error)
		(title "Invalid Statement")
		(region (start 12 5) (end 14 6))
		(headline
			(reflow "The statement ")
			(annotated code "while")
			(reflow " is not allowed in an associated block."))
		(document
			(reflow "Only associated values, type declarations, and type annotations are allowed in an associated block.")
			(line-break)
			(line-break)
			(source-region (file "assoc_invalid_statements.md") (start 12 5) (end 14 6) (annotation error) (line-text "    while 1 == 2 {\n        dbg 3\n    }"))))
	(report
		(severity runtime_error)
		(title "Invalid Statement")
		(region (start 16 5) (end 16 10))
		(headline
			(reflow "The statement ")
			(annotated code "break")
			(reflow " is not allowed in an associated block."))
		(document
			(reflow "Only associated values, type declarations, and type annotations are allowed in an associated block.")
			(line-break)
			(line-break)
			(source-region (file "assoc_invalid_statements.md") (start 16 5) (end 16 10) (annotation error) (line-text "    break")))))
~~~
# TOKENS
~~~zig
UpperIdent,OpColonEqual,OpenSquare,CloseSquare,Dot,OpenCurly,
KwDbg,Int,
KwCrash,StringStart,StringPart,StringEnd,
KwReturn,Int,
KwFor,LowerIdent,KwIn,OpenSquare,Int,Comma,Int,CloseSquare,OpenCurly,
KwDbg,LowerIdent,
CloseCurly,
KwWhile,Int,OpEquals,Int,OpenCurly,
KwDbg,Int,
CloseCurly,
KwBreak,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-type-decl
			(header (name "Counter")
				(args))
			(ty-tag-union
				(tags))
			(associated
				(s-dbg
					(e-int (raw "5")))
				(s-crash
					(e-string
						(e-string-part (raw "boom"))))
				(s-return
					(e-int (raw "5")))
				(s-for
					(p-ident (raw "x"))
					(e-list
						(e-int (raw "1"))
						(e-int (raw "2")))
					(e-block
						(statements
							(s-dbg
								(e-ident (raw "x"))))))
				(s-while
					(e-binop (op "==")
						(e-int (raw "1"))
						(e-int (raw "2")))
					(e-block
						(statements
							(s-dbg
								(e-int (raw "3"))))))
				(s-break)))))
~~~
# FORMATTED
~~~roc
Counter := [].{
	dbg 5

	crash "boom"

	return 5

	for x in [1, 2] {
		dbg x
	}

	while 1 == 2 {
		dbg 3
	}

	break
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(s-nominal-decl
		(ty-header (name "Counter"))
		(ty-tag-union)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(type_decls
		(nominal (type "Counter")
			(ty-header (name "Counter"))))
	(expressions))
~~~
