# META
~~~ini
description=A name bound more than once within one pattern or one lambda's arguments is an error, while or-pattern alternatives and nested scopes may reuse names
type=snippet
~~~
# SOURCE
~~~roc
same_tuple = |pair| match pair {
    (from, from) => True
    _ => False
}

same_record = |rec| match rec {
    { x, y: x } => x
}

same_as = |pair| match pair {
    (a, _) as a => a
}

same_rest = |items| match items {
    [first, .. as first] => first
    _ => []
}

same_args = |a, a| a

alternatives = |result| match result {
    Ok(n) | Err(n) => n
}

nested_shadow = |n| |n| n

(dup, dup) = (1, 2)

{ first_part, second_part: (inner, first_part) } = { first_part: 1, second_part: (2, 3) }

(outer, nested) = (1, |pair| match pair {
    (outer, _) => outer
})
~~~
# EXPECTED
DUPLICATE NAME IN PATTERN - pattern_duplicate_binder.md:2:12:2:16
UNUSED VARIABLE - pattern_duplicate_binder.md:2:6:2:10
DUPLICATE NAME IN PATTERN - pattern_duplicate_binder.md:7:13:7:14
DUPLICATE NAME IN PATTERN - pattern_duplicate_binder.md:11:15:11:16
DUPLICATE NAME IN PATTERN - pattern_duplicate_binder.md:15:19:15:24
DUPLICATE NAME IN PATTERN - pattern_duplicate_binder.md:19:17:19:18
DUPLICATE DEFINITION - pattern_duplicate_binder.md:25:22:25:23
UNUSED VARIABLE - pattern_duplicate_binder.md:25:18:25:19
DUPLICATE NAME IN PATTERN - pattern_duplicate_binder.md:27:7:27:10
DUPLICATE NAME IN PATTERN - pattern_duplicate_binder.md:29:36:29:46
DUPLICATE DEFINITION - pattern_duplicate_binder.md:32:6:32:11
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Duplicate Name In Pattern")
		(region (start 2 12) (end 2 16))
		(headline
			(reflow "The name ")
			(annotated symbol-unqualified "from")
			(reflow " is bound more than once in this pattern."))
		(document
			(source-region (file "pattern_duplicate_binder.md") (start 2 12) (end 2 16) (annotation error) (line-text "    (from, from) => True"))
			(line-break)
			(reflow "It was first bound here:")
			(line-break)
			(source-region (file "pattern_duplicate_binder.md") (start 2 6) (end 2 10) (annotation dim) (line-text "    (from, from) => True"))
			(line-break)
			(reflow "Each name in a pattern must be different. To check whether two values are equal, give them different names and compare them in a guard:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "(a, b) if a == b => ...")
			(annotation-end)))
	(report
		(severity warning)
		(title "Unused Variable")
		(region (start 2 6) (end 2 10))
		(headline
			(reflow "Variable ")
			(annotated symbol-unqualified "from")
			(reflow " is defined here and then never used:"))
		(document
			(reflow "If you don't need this variable, prefix it with an underscore like ")
			(annotated symbol-unqualified "_from")
			(reflow " to suppress this warning.")
			(line-break)
			(source-region (file "pattern_duplicate_binder.md") (start 2 6) (end 2 10) (annotation error) (line-text "    (from, from) => True"))))
	(report
		(severity runtime_error)
		(title "Duplicate Name In Pattern")
		(region (start 7 13) (end 7 14))
		(headline
			(reflow "The name ")
			(annotated symbol-unqualified "x")
			(reflow " is bound more than once in this pattern."))
		(document
			(source-region (file "pattern_duplicate_binder.md") (start 7 13) (end 7 14) (annotation error) (line-text "    { x, y: x } => x"))
			(line-break)
			(reflow "It was first bound here:")
			(line-break)
			(source-region (file "pattern_duplicate_binder.md") (start 7 7) (end 7 8) (annotation dim) (line-text "    { x, y: x } => x"))
			(line-break)
			(reflow "Each name in a pattern must be different. To check whether two values are equal, give them different names and compare them in a guard:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "(a, b) if a == b => ...")
			(annotation-end)))
	(report
		(severity runtime_error)
		(title "Duplicate Name In Pattern")
		(region (start 11 15) (end 11 16))
		(headline
			(reflow "The name ")
			(annotated symbol-unqualified "a")
			(reflow " is bound more than once in this pattern."))
		(document
			(source-region (file "pattern_duplicate_binder.md") (start 11 15) (end 11 16) (annotation error) (line-text "    (a, _) as a => a"))
			(line-break)
			(reflow "It was first bound here:")
			(line-break)
			(source-region (file "pattern_duplicate_binder.md") (start 11 6) (end 11 7) (annotation dim) (line-text "    (a, _) as a => a"))
			(line-break)
			(reflow "Each name in a pattern must be different. To check whether two values are equal, give them different names and compare them in a guard:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "(a, b) if a == b => ...")
			(annotation-end)))
	(report
		(severity runtime_error)
		(title "Duplicate Name In Pattern")
		(region (start 15 19) (end 15 24))
		(headline
			(reflow "The name ")
			(annotated symbol-unqualified "first")
			(reflow " is bound more than once in this pattern."))
		(document
			(source-region (file "pattern_duplicate_binder.md") (start 15 19) (end 15 24) (annotation error) (line-text "    [first, .. as first] => first"))
			(line-break)
			(reflow "It was first bound here:")
			(line-break)
			(source-region (file "pattern_duplicate_binder.md") (start 15 6) (end 15 11) (annotation dim) (line-text "    [first, .. as first] => first"))
			(line-break)
			(reflow "Each name in a pattern must be different. To check whether two values are equal, give them different names and compare them in a guard:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "(a, b) if a == b => ...")
			(annotation-end)))
	(report
		(severity runtime_error)
		(title "Duplicate Name In Pattern")
		(region (start 19 17) (end 19 18))
		(headline
			(reflow "The name ")
			(annotated symbol-unqualified "a")
			(reflow " is bound more than once in this pattern."))
		(document
			(source-region (file "pattern_duplicate_binder.md") (start 19 17) (end 19 18) (annotation error) (line-text "same_args = |a, a| a"))
			(line-break)
			(reflow "It was first bound here:")
			(line-break)
			(source-region (file "pattern_duplicate_binder.md") (start 19 14) (end 19 15) (annotation dim) (line-text "same_args = |a, a| a"))
			(line-break)
			(reflow "Each name in a pattern must be different. To check whether two values are equal, give them different names and compare them in a guard:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "(a, b) if a == b => ...")
			(annotation-end)))
	(report
		(severity warning)
		(title "Duplicate Definition")
		(region (start 25 22) (end 25 23))
		(headline
			(reflow "The name ")
			(annotated symbol-unqualified "n")
			(reflow " is being redeclared here:"))
		(document
			(source-region (file "pattern_duplicate_binder.md") (start 25 22) (end 25 23) (annotation error) (line-text "nested_shadow = |n| |n| n"))
			(line-break)
			(reflow "In this scope, ")
			(annotated symbol-unqualified "n")
			(reflow " was already defined in ")
			(source-location
				(file "pattern_duplicate_binder.md")
				(line 25)
				(column 18))
			(reflow ":")
			(line-break)
			(source-region (file "pattern_duplicate_binder.md") (start 25 18) (end 25 19) (annotation dim) (line-text "nested_shadow = |n| |n| n"))))
	(report
		(severity warning)
		(title "Unused Variable")
		(region (start 25 18) (end 25 19))
		(headline
			(reflow "Variable ")
			(annotated symbol-unqualified "n")
			(reflow " is defined here and then never used:"))
		(document
			(reflow "If you don't need this variable, prefix it with an underscore like ")
			(annotated symbol-unqualified "_n")
			(reflow " to suppress this warning.")
			(line-break)
			(source-region (file "pattern_duplicate_binder.md") (start 25 18) (end 25 19) (annotation error) (line-text "nested_shadow = |n| |n| n"))))
	(report
		(severity runtime_error)
		(title "Duplicate Name In Pattern")
		(region (start 27 7) (end 27 10))
		(headline
			(reflow "The name ")
			(annotated symbol-unqualified "dup")
			(reflow " is bound more than once in this pattern."))
		(document
			(source-region (file "pattern_duplicate_binder.md") (start 27 7) (end 27 10) (annotation error) (line-text "(dup, dup) = (1, 2)"))
			(line-break)
			(reflow "It was first bound here:")
			(line-break)
			(source-region (file "pattern_duplicate_binder.md") (start 27 2) (end 27 5) (annotation dim) (line-text "(dup, dup) = (1, 2)"))
			(line-break)
			(reflow "Each name in a pattern must be different. To check whether two values are equal, give them different names and compare them in a guard:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "(a, b) if a == b => ...")
			(annotation-end)))
	(report
		(severity runtime_error)
		(title "Duplicate Name In Pattern")
		(region (start 29 36) (end 29 46))
		(headline
			(reflow "The name ")
			(annotated symbol-unqualified "first_part")
			(reflow " is bound more than once in this pattern."))
		(document
			(source-region (file "pattern_duplicate_binder.md") (start 29 36) (end 29 46) (annotation error) (line-text "{ first_part, second_part: (inner, first_part) } = { first_part: 1, second_part: (2, 3) }"))
			(line-break)
			(reflow "It was first bound here:")
			(line-break)
			(source-region (file "pattern_duplicate_binder.md") (start 29 3) (end 29 13) (annotation dim) (line-text "{ first_part, second_part: (inner, first_part) } = { first_part: 1, second_part: (2, 3) }"))
			(line-break)
			(reflow "Each name in a pattern must be different. To check whether two values are equal, give them different names and compare them in a guard:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "(a, b) if a == b => ...")
			(annotation-end)))
	(report
		(severity warning)
		(title "Duplicate Definition")
		(region (start 32 6) (end 32 11))
		(headline
			(reflow "The name ")
			(annotated symbol-unqualified "outer")
			(reflow " is being redeclared here:"))
		(document
			(source-region (file "pattern_duplicate_binder.md") (start 32 6) (end 32 11) (annotation error) (line-text "    (outer, _) => outer"))
			(line-break)
			(reflow "In this scope, ")
			(annotated symbol-unqualified "outer")
			(reflow " was already defined in ")
			(source-location
				(file "pattern_duplicate_binder.md")
				(line 31)
				(column 2))
			(reflow ":")
			(line-break)
			(source-region (file "pattern_duplicate_binder.md") (start 31 2) (end 31 7) (annotation dim) (line-text "(outer, nested) = (1, |pair| match pair {")))))
~~~
# TOKENS
~~~zig
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,KwMatch,LowerIdent,OpenCurly,
OpenRound,LowerIdent,Comma,LowerIdent,CloseRound,OpFatArrow,UpperIdent,
Underscore,OpFatArrow,UpperIdent,
CloseCurly,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,KwMatch,LowerIdent,OpenCurly,
OpenCurly,LowerIdent,Comma,LowerIdent,OpColon,LowerIdent,CloseCurly,OpFatArrow,LowerIdent,
CloseCurly,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,KwMatch,LowerIdent,OpenCurly,
OpenRound,LowerIdent,Comma,Underscore,CloseRound,KwAs,LowerIdent,OpFatArrow,LowerIdent,
CloseCurly,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,KwMatch,LowerIdent,OpenCurly,
OpenSquare,LowerIdent,Comma,DoubleDot,KwAs,LowerIdent,CloseSquare,OpFatArrow,LowerIdent,
Underscore,OpFatArrow,OpenSquare,CloseSquare,
CloseCurly,
LowerIdent,OpAssign,OpBar,LowerIdent,Comma,LowerIdent,OpBar,LowerIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,KwMatch,LowerIdent,OpenCurly,
UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpBar,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpFatArrow,LowerIdent,
CloseCurly,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpBar,LowerIdent,OpBar,LowerIdent,
OpenRound,LowerIdent,Comma,LowerIdent,CloseRound,OpAssign,OpenRound,Int,Comma,Int,CloseRound,
OpenCurly,LowerIdent,Comma,LowerIdent,OpColon,OpenRound,LowerIdent,Comma,LowerIdent,CloseRound,CloseCurly,OpAssign,OpenCurly,LowerIdent,OpColon,Int,Comma,LowerIdent,OpColon,OpenRound,Int,Comma,Int,CloseRound,CloseCurly,
OpenRound,LowerIdent,Comma,LowerIdent,CloseRound,OpAssign,OpenRound,Int,Comma,OpBar,LowerIdent,OpBar,KwMatch,LowerIdent,OpenCurly,
OpenRound,LowerIdent,Comma,Underscore,CloseRound,OpFatArrow,LowerIdent,
CloseCurly,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-decl
			(p-ident (raw "same_tuple"))
			(e-lambda
				(args
					(p-ident (raw "pair")))
				(e-match
					(e-ident (raw "pair"))
					(branches
						(branch
							(p-tuple
								(p-ident (raw "from"))
								(p-ident (raw "from")))
							(e-tag (raw "True")))
						(branch
							(p-underscore)
							(e-tag (raw "False")))))))
		(s-decl
			(p-ident (raw "same_record"))
			(e-lambda
				(args
					(p-ident (raw "rec")))
				(e-match
					(e-ident (raw "rec"))
					(branches
						(branch
							(p-record
								(field (name "x") (rest false))
								(field (name "y") (rest false)
									(p-ident (raw "x"))))
							(e-ident (raw "x")))))))
		(s-decl
			(p-ident (raw "same_as"))
			(e-lambda
				(args
					(p-ident (raw "pair")))
				(e-match
					(e-ident (raw "pair"))
					(branches
						(branch
							(p-as (name "a")
								(p-tuple
									(p-ident (raw "a"))
									(p-underscore)))
							(e-ident (raw "a")))))))
		(s-decl
			(p-ident (raw "same_rest"))
			(e-lambda
				(args
					(p-ident (raw "items")))
				(e-match
					(e-ident (raw "items"))
					(branches
						(branch
							(p-list
								(p-ident (raw "first"))
								(p-list-rest (name "first")))
							(e-ident (raw "first")))
						(branch
							(p-underscore)
							(e-list))))))
		(s-decl
			(p-ident (raw "same_args"))
			(e-lambda
				(args
					(p-ident (raw "a"))
					(p-ident (raw "a")))
				(e-ident (raw "a"))))
		(s-decl
			(p-ident (raw "alternatives"))
			(e-lambda
				(args
					(p-ident (raw "result")))
				(e-match
					(e-ident (raw "result"))
					(branches
						(branch
							(p-alternatives
								(p-tag (raw "Ok")
									(p-ident (raw "n")))
								(p-tag (raw "Err")
									(p-ident (raw "n"))))
							(e-ident (raw "n")))))))
		(s-decl
			(p-ident (raw "nested_shadow"))
			(e-lambda
				(args
					(p-ident (raw "n")))
				(e-lambda
					(args
						(p-ident (raw "n")))
					(e-ident (raw "n")))))
		(s-decl
			(p-tuple
				(p-ident (raw "dup"))
				(p-ident (raw "dup")))
			(e-tuple
				(e-int (raw "1"))
				(e-int (raw "2"))))
		(s-decl
			(p-record
				(field (name "first_part") (rest false))
				(field (name "second_part") (rest false)
					(p-tuple
						(p-ident (raw "inner"))
						(p-ident (raw "first_part")))))
			(e-record
				(field (field "first_part")
					(e-int (raw "1")))
				(field (field "second_part")
					(e-tuple
						(e-int (raw "2"))
						(e-int (raw "3"))))))
		(s-decl
			(p-tuple
				(p-ident (raw "outer"))
				(p-ident (raw "nested")))
			(e-tuple
				(e-int (raw "1"))
				(e-lambda
					(args
						(p-ident (raw "pair")))
					(e-match
						(e-ident (raw "pair"))
						(branches
							(branch
								(p-tuple
									(p-ident (raw "outer"))
									(p-underscore))
								(e-ident (raw "outer"))))))))))
~~~
# FORMATTED
~~~roc
same_tuple = |pair| match pair {
	(from, from) => True
	_ => False
}

same_record = |rec| match rec {
	{ x, y: x } => x
}

same_as = |pair| match pair {
	(a, _) as a => a
}

same_rest = |items| match items {
	[first, .. as first] => first
	_ => []
}

same_args = |a, a| a

alternatives = |result| match result {
	Ok(n) | Err(n) => n
}

nested_shadow = |n| |n| n

(dup, dup) = (1, 2)

{ first_part, second_part: (inner, first_part) } = { first_part: 1, second_part: (2, 3) }

(outer, nested) = (
	1,
	|pair| match pair {
		(outer, _) => outer
	},
)
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "same_tuple"))
		(e-runtime-error (tag "erroneous_value_expr")))
	(d-let
		(p-assign (ident "same_record"))
		(e-runtime-error (tag "erroneous_value_expr")))
	(d-let
		(p-assign (ident "same_as"))
		(e-runtime-error (tag "erroneous_value_expr")))
	(d-let
		(p-assign (ident "same_rest"))
		(e-runtime-error (tag "erroneous_value_expr")))
	(d-let
		(p-assign (ident "same_args"))
		(e-runtime-error (tag "erroneous_value_expr")))
	(d-let
		(p-assign (ident "alternatives"))
		(e-lambda
			(args
				(p-assign (ident "result")))
			(e-match
				(match
					(cond
						(e-lookup-local
							(p-assign (ident "result"))))
					(branches
						(branch
							(patterns
								(pattern (degenerate false)
									(p-applied-tag))
								(pattern (degenerate false)
									(p-applied-tag)))
							(value
								(e-lookup-local
									(p-assign (ident "n"))))))))))
	(d-let
		(p-assign (ident "nested_shadow"))
		(e-lambda
			(args
				(p-assign (ident "n")))
			(e-lambda
				(args
					(p-assign (ident "n")))
				(e-lookup-local
					(p-assign (ident "n"))))))
	(d-let
		(p-assign (ident "dup"))
		(e-num (value "1")))
	(d-let
		invalid-pattern
		(e-runtime-error (tag "erroneous_value_expr")))
	(d-let
		(p-assign (ident "first_part"))
		(e-num (value "1")))
	(d-let
		(p-assign (ident "inner"))
		(e-num (value "2")))
	(d-let
		invalid-pattern
		(e-runtime-error (tag "erroneous_value_expr")))
	(d-let
		(p-assign (ident "outer"))
		(e-num (value "1")))
	(d-let
		(p-assign (ident "nested"))
		(e-lambda
			(args
				(p-assign (ident "pair")))
			(e-match
				(match
					(cond
						(e-lookup-local
							(p-assign (ident "pair"))))
					(branches
						(branch
							(patterns
								(pattern (degenerate false)
									(p-tuple
										(patterns
											(p-assign (ident "outer"))
											(p-underscore)))))
							(value
								(e-lookup-local
									(p-assign (ident "outer")))))))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "(Error, Error) -> [False, True]"))
		(patt (type "{ x: Error, y: Error } -> Error"))
		(patt (type "Error -> _ret"))
		(patt (type "List(Error) -> Error"))
		(patt (type "b, Error -> b"))
		(patt (type "[Err(b), Ok(b)] -> b"))
		(patt (type "_arg -> (b -> b)"))
		(patt (type "Dec"))
		(patt (type "Dec"))
		(patt (type "Dec"))
		(patt (type "Dec"))
		(patt (type "(b, _field) -> b")))
	(expressions
		(expr (type "(Error, Error) -> [False, True]"))
		(expr (type "{ x: Error, y: Error } -> Error"))
		(expr (type "Error -> _ret"))
		(expr (type "List(Error) -> Error"))
		(expr (type "b, Error -> b"))
		(expr (type "[Err(b), Ok(b)] -> b"))
		(expr (type "_arg -> (b -> b)"))
		(expr (type "Dec"))
		(expr (type "Dec"))
		(expr (type "Dec"))
		(expr (type "Dec"))
		(expr (type "Dec"))
		(expr (type "Dec"))
		(expr (type "(b, _field) -> b"))))
~~~
