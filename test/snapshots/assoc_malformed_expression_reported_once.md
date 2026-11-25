# META
~~~ini
description=A token that cannot start an expression in an associated block gets its own report and no second report for being an expression statement
type=file:Once.roc
~~~
# SOURCE
~~~roc
Once := [A].{
    x : U64 -> U64,
    y = 1
}
~~~
# EXPECTED
UNEXPECTED EXPRESSION SYNTAX - assoc_malformed_expression_reported_once.md:2:19:2:20
DECLARATION HAS NO VALUE - assoc_malformed_expression_reported_once.md:2:5:2:19
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Unexpected Expression Syntax")
		(region (start 2 19) (end 2 20))
		(headline
			(reflow "I was parsing an expression, and this token cannot start an expression here."))
		(document
			(reflow "Expressions can be names, literals, tags, records, lists, tuples, lambdas, blocks, conditionals, matches, or function calls.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "add(1, 2)")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code ",")
			(text " here.")
			(line-break)
			(reflow "A comma separates items, but there must be a valid item on both sides of it.")
			(line-break)
			(line-break)
			(source-region (file "assoc_malformed_expression_reported_once.md") (start 2 19) (end 2 20) (annotation error) (line-text "    x : U64 -> U64,"))))
	(report
		(severity warning)
		(title "Declaration Has No Value")
		(region (start 2 5) (end 2 19))
		(headline
			(reflow "This declaration has a type annotation but no implementation."))
		(document
			(source-region (file "assoc_malformed_expression_reported_once.md") (start 2 5) (end 2 19) (annotation error) (line-text "    x : U64 -> U64,"))
			(line-break)
			(line-break)
			(reflow "Add a value body here, or put hosted functions in a platform type mod so they are published through the host boundary."))))
~~~
# TOKENS
~~~zig
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,Comma,
LowerIdent,OpAssign,Int,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-type-decl
			(header (name "Once")
				(args))
			(ty-tag-union
				(tags
					(ty (name "A"))))
			(associated
				(s-type-anno (name "x")
					(ty-fn
						(ty (name "U64"))
						(ty (name "U64"))))
				(e-malformed (reason "expr_unexpected_token"))
				(s-decl
					(p-ident (raw "y"))
					(e-int (raw "1")))))))
~~~
# FORMATTED
~~~roc
Once := [A].{
	x : U64 -> U64

	y = 1
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "Once.x"))
		(e-anno-only)
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "U64") (builtin))
				(ty-lookup (name "U64") (builtin)))))
	(d-let
		(p-assign (ident "Once.y"))
		(e-num (value "1")))
	(s-nominal-decl
		(ty-header (name "Once"))
		(ty-tag-union
			(ty-tag-name (name "A")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "U64 -> U64"))
		(patt (type "Dec")))
	(type_decls
		(nominal (type "Once")
			(ty-header (name "Once"))))
	(expressions
		(expr (type "U64 -> U64"))
		(expr (type "Dec"))))
~~~
