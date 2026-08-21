# META
~~~ini
description=Example if-then-else statement
type=snippet
~~~
# SOURCE
~~~roc
foo = if 1 A

    else {
	"hello"
    }
~~~
# EXPECTED
TYPE MISMATCH - if_then_else_simple_file.md:1:10:1:11
MISSING METHOD - if_then_else_simple_file.md:4:2:4:9
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Type Mismatch")
		(region (start 1 10) (end 1 11))
		(headline
			(reflow "This number is being used where a non-number type is needed."))
		(document
			(source-region (file "if_then_else_simple_file.md") (start 1 10) (end 1 11) (annotation error) (line-text "foo = if 1 A"))
			(line-break)
			(reflow "Other code expects this to have the type:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "Bool")
			(annotation-end)))
	(report
		(severity runtime_error)
		(title "Missing Method")
		(region (start 4 2) (end 4 9))
		(headline
			(reflow "This")
			(reflow " ")
			(annotated code "from_quote")
			(reflow " ")
			(reflow "method is being called on a value whose type doesn't have that method."))
		(document
			(source-region (file "if_then_else_simple_file.md") (start 4 2) (end 4 9) (annotation error) (line-text "\t\"hello\""))
			(line-break)
			(reflow "The value's type, which does not have a method named ")
			(annotated code "from_quote")
			(reflow ",")
			(reflow " ")
			(reflow "is:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "[A, ..]")
			(annotation-end))))
~~~
# TOKENS
~~~zig
LowerIdent,OpAssign,KwIf,Int,UpperIdent,
KwElse,OpenCurly,
StringStart,StringPart,StringEnd,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-decl
			(p-ident (raw "foo"))
			(e-if-then-else
				(e-int (raw "1"))
				(e-tag (raw "A"))
				(e-block
					(statements
						(e-string
							(e-string-part (raw "hello")))))))))
~~~
# FORMATTED
~~~roc
foo = if 1 A

	else {
		"hello"
	}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "foo"))
		(e-if
			(if-branches
				(if-branch
					(e-runtime-error (tag "erroneous_value_expr"))
					(e-tag (name "A"))))
			(if-else
				(e-block
					(e-runtime-error (tag "erroneous_value_expr")))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "[A, ..]")))
	(expressions
		(expr (type "[A, ..]"))))
~~~
