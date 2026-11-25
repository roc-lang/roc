# META
~~~ini
description=An expression statement in an associated block that is never closed is reported alongside the missing brace (issue 12802)
type=file:Unclosed.roc
~~~
# SOURCE
~~~roc
Unclosed := [A].{
    x = 1
    junk
~~~
# EXPECTED
UNEXPECTED ASSOCIATED EXPRESSION - assoc_stray_expression_unclosed.md:3:5:3:9
EXPECTED CLOSING BRACE - assoc_stray_expression_unclosed.md:4:1:4:1
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Unexpected Associated Expression")
		(region (start 3 5) (end 3 9))
		(headline
			(reflow "I was parsing associated items for a nominal type, and I found a plain expression."))
		(document
			(reflow "Associated item blocks can contain associated types and values. Remove this expression or turn it into a named associated value.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "Id := [Id(U64)].{")
			(line-break)
			(indent 1)
			(text "    zero = Id(0)")
			(line-break)
			(indent 1)
			(text "}")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "junk")
			(text " here.")
			(line-break)
			(reflow "Names that start with lowercase letters are value names or record field names, depending on the surrounding syntax.")
			(line-break)
			(line-break)
			(source-region (file "assoc_stray_expression_unclosed.md") (start 3 5) (end 3 9) (annotation error) (line-text "    junk"))))
	(report
		(severity runtime_error)
		(title "Expected Closing Brace")
		(region (start 4 1) (end 4 1))
		(headline
			(reflow "I was parsing a block expression, and I expected `}` before the file ended."))
		(document
			(reflow "Close the block after its final statement or expression.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "{")
			(line-break)
			(indent 1)
			(text "    answer = 42")
			(line-break)
			(indent 1)
			(text "    answer")
			(line-break)
			(indent 1)
			(text "}")
			(annotation-end)
			(line-break)
			(line-break)
			(reflow "I reached the end of the file before this construct was complete.")
			(line-break)
			(line-break)
			(source-region (file "assoc_stray_expression_unclosed.md") (start 4 1) (end 4 1) (annotation error) (line-text "")))))
~~~
# TOKENS
~~~zig
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,Int,
LowerIdent,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-type-decl
			(header (name "Unclosed")
				(args))
			(ty-tag-union
				(tags
					(ty (name "A"))))
			(associated
				(s-decl
					(p-ident (raw "x"))
					(e-int (raw "1")))
				(e-ident (raw "junk"))))))
~~~
# FORMATTED
~~~roc
Unclosed := [A].{
	x = 1
	junk
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "Unclosed.x"))
		(e-num (value "1")))
	(s-nominal-decl
		(ty-header (name "Unclosed"))
		(ty-tag-union
			(ty-tag-name (name "A")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Dec")))
	(type_decls
		(nominal (type "Unclosed")
			(ty-header (name "Unclosed"))))
	(expressions
		(expr (type "Dec"))))
~~~
