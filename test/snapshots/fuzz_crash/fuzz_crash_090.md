# META
~~~ini
description=Issue #10096: Invalid formatting - multiline function literal in tuple call
type=file
~~~
# SOURCE
~~~roc
e={({\\
.{f}{})
~~~
# EXPECTED
UNEXPECTED EXPRESSION SYNTAX - fuzz_crash_090.md:2:7:2:8
EXPECTED CLOSING BRACE - fuzz_crash_090.md:3:1:3:1
EXPECTED TUPLE SEPARATOR - fuzz_crash_090.md:3:1:3:1
EXPECTED CLOSING BRACE - fuzz_crash_090.md:3:1:3:1
UNRECOGNIZED SYNTAX - fuzz_crash_090.md:1:1:1:1
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Unexpected Expression Syntax")
		(region (start 2 7) (end 2 8))
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
			(annotated code ")")
			(text " here.")
			(line-break)
			(reflow "This closes the current construct, so the parser was looking for the missing item before it.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_090.md") (start 2 7) (end 2 8) (annotation error) (line-text ".{f}{})"))))
	(report
		(severity runtime_error)
		(title "Expected Closing Brace")
		(region (start 3 1) (end 3 1))
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
			(source-region (file "fuzz_crash_090.md") (start 3 1) (end 3 1) (annotation error) (line-text ""))))
	(report
		(severity runtime_error)
		(title "Expected Tuple Separator")
		(region (start 3 1) (end 3 1))
		(headline
			(reflow "I was parsing a parenthesized expression or tuple, and I expected `,` or `)`."))
		(document
			(reflow "Separate tuple elements with commas and close the tuple or parenthesized expression with ")
			(annotated code ")")
			(reflow ".")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "(x, y)")
			(annotation-end)
			(line-break)
			(line-break)
			(reflow "I reached the end of the file before this construct was complete.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_090.md") (start 3 1) (end 3 1) (annotation error) (line-text ""))))
	(report
		(severity runtime_error)
		(title "Expected Closing Brace")
		(region (start 3 1) (end 3 1))
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
			(source-region (file "fuzz_crash_090.md") (start 3 1) (end 3 1) (annotation error) (line-text ""))))
	(report
		(severity runtime_error)
		(title "Unrecognized Syntax")
		(region (start 1 1) (end 1 1))
		(headline
			(reflow "I don't recognize this syntax."))
		(document
			(source-region (file "fuzz_crash_090.md") (start 1 1) (end 1 1) (annotation error) (line-text "e={({\\\\"))
			(line-break)
			(reflow "This might be a syntax error, an unsupported language feature, or a typo."))))
~~~
# TOKENS
~~~zig
LowerIdent,OpAssign,OpenCurly,NoSpaceOpenRound,OpenCurly,MultilineStringStart,StringPart,
Dot,OpenCurly,LowerIdent,CloseCurly,OpenCurly,CloseCurly,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-decl
			(p-ident (raw "e"))
			(e-block
				(statements
					(e-malformed (reason "expected_expr_close_round_or_comma")))))))
~~~
# FORMATTED
~~~roc
e = {
	
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "e"))
		(e-runtime-error (tag "erroneous_value_expr"))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Error")))
	(expressions
		(expr (type "Error"))))
~~~
