# META
~~~ini
description=fuzz crash, unterminated single quote
type=snippet
~~~
# SOURCE
~~~roc
LocalStatus :lue => Loc= [Pending, Complete]

olor : _ -> tus
olor = |color| { import Color.RGB

    match color { RGB => LocalStatus.Pending
Green => LocalStatus-Complete
  B.Blue => LocalStatus.Pending
    }
}
~~~
# EXPECTED
UNEXPECTED STATEMENT - fuzz_crash_032.md:1:24:1:25
UNEXPECTED STATEMENT - fuzz_crash_032.md:1:26:1:27
TYPE APPLICATION NEEDS PARENTHESES - fuzz_crash_032.md:1:34:1:35
TYPE APPLICATION NEEDS PARENTHESES - fuzz_crash_032.md:1:44:1:45
IMPORT MUST BE TOP LEVEL - fuzz_crash_032.md:4:18:4:24
UNEXPECTED PATTERN SYNTAX - fuzz_crash_032.md:7:21:7:22
MISSING MATCH ARROW - fuzz_crash_032.md:7:22:7:22
UNDECLARED TYPE VARIABLE - fuzz_crash_032.md:1:14:1:17
UNDECLARED TYPE - fuzz_crash_032.md:1:21:1:24
UNDECLARED TYPE - fuzz_crash_032.md:4:25:4:30
EXPECTED NOMINAL TYPE - fuzz_crash_032.md:6:26:6:37
INVALID PATTERN - :0:0:0:0
UNDECLARED TYPE - fuzz_crash_032.md:8:3:8:4
EXPECTED NOMINAL TYPE - fuzz_crash_032.md:8:13:8:24
TYPE MISMATCH - fuzz_crash_032.md:7:10:7:21
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 1 24) (end 1 25))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "=")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_032.md") (start 1 24) (end 1 25) (annotation error) (line-text "LocalStatus :lue => Loc= [Pending, Complete]"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 1 26) (end 1 27))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "[")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_032.md") (start 1 26) (end 1 27) (annotation error) (line-text "LocalStatus :lue => Loc= [Pending, Complete]"))))
	(report
		(severity runtime_error)
		(title "Type Application Needs Parentheses")
		(region (start 1 34) (end 1 35))
		(headline
			(reflow "I was parsing a type annotation, and I found a type argument without parentheses."))
		(document
			(reflow "Roc type applications use parentheses around their arguments. Write ")
			(annotated code "List(U8)")
			(reflow ", not ")
			(annotated code "List U8")
			(reflow ".")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "List(U8)")
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
			(source-region (file "fuzz_crash_032.md") (start 1 34) (end 1 35) (annotation error) (line-text "LocalStatus :lue => Loc= [Pending, Complete]"))))
	(report
		(severity runtime_error)
		(title "Type Application Needs Parentheses")
		(region (start 1 44) (end 1 45))
		(headline
			(reflow "I was parsing a type annotation, and I found a type argument without parentheses."))
		(document
			(reflow "Roc type applications use parentheses around their arguments. Write ")
			(annotated code "List(U8)")
			(reflow ", not ")
			(annotated code "List U8")
			(reflow ".")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "List(U8)")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "]")
			(text " here.")
			(line-break)
			(reflow "This closes the current construct, so the parser was looking for the missing item before it.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_032.md") (start 1 44) (end 1 45) (annotation error) (line-text "LocalStatus :lue => Loc= [Pending, Complete]"))))
	(report
		(severity runtime_error)
		(title "Import Must Be Top Level")
		(region (start 4 18) (end 4 24))
		(headline
			(reflow "I was parsing an import, but imports are only allowed at the top level."))
		(document
			(reflow "Move this import after the mod header and before declarations or executable statements.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "import Json")
			(line-break)
			(line-break)
			(indent 1)
			(text "main = 1")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "import")
			(text " here.")
			(line-break)
			(reflow "That word is reserved by Roc, so it cannot be used as a name in this position.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_032.md") (start 4 18) (end 4 24) (annotation error) (line-text "olor = |color| { import Color.RGB"))))
	(report
		(severity runtime_error)
		(title "Unexpected Pattern Syntax")
		(region (start 7 21) (end 7 22))
		(headline
			(reflow "I was parsing a pattern, and this token cannot start a pattern here."))
		(document
			(reflow "Patterns can be lowercase names, tags, literals, lists, records, tuples, underscores, or nested patterns.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "{ name, age }")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "-")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_032.md") (start 7 21) (end 7 22) (annotation error) (line-text "Green => LocalStatus-Complete"))))
	(report
		(severity runtime_error)
		(title "Missing Match Arrow")
		(region (start 7 22) (end 7 22))
		(headline
			(reflow "I was parsing a match branch, and I expected `=>` before the branch body."))
		(document
			(reflow "Add ")
			(annotated code "=>")
			(reflow " after the pattern or guard.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "Err(msg) => crash msg")
			(annotation-end)
			(line-break)
			(line-break)
			(reflow "I reached the end of the file before this construct was complete.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_032.md") (start 7 22) (end 7 22) (annotation error) (line-text "Green => LocalStatus-Complete"))))
	(report
		(severity runtime_error)
		(title "Undeclared Type Variable")
		(region (start 1 14) (end 1 17))
		(headline
			(reflow "The type variable ")
			(annotated code "lue")
			(reflow " is not declared in this scope."))
		(document
			(reflow "Type variables must be introduced in a type annotation before they can be used.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_032.md") (start 1 14) (end 1 17) (annotation error) (line-text "LocalStatus :lue => Loc= [Pending, Complete]"))))
	(report
		(severity runtime_error)
		(title "Undeclared Type")
		(region (start 1 21) (end 1 24))
		(headline
			(reflow "The type ")
			(annotated code "Loc")
			(reflow " is not declared in this scope."))
		(document
			(source-region (file "fuzz_crash_032.md") (start 1 21) (end 1 24) (annotation error) (line-text "LocalStatus :lue => Loc= [Pending, Complete]"))))
	(report
		(severity runtime_error)
		(title "Undeclared Type")
		(region (start 4 25) (end 4 30))
		(headline
			(reflow "The type ")
			(annotated code "Color")
			(reflow " is not declared in this scope."))
		(document
			(source-region (file "fuzz_crash_032.md") (start 4 25) (end 4 30) (annotation error) (line-text "olor = |color| { import Color.RGB"))))
	(report
		(severity runtime_error)
		(title "Expected Nominal Type")
		(region (start 6 26) (end 6 37))
		(headline
			(reflow "You are using the type ")
			(annotated code "LocalStatus")
			(reflow " like a nominal type, but it is an alias."))
		(document
			(source-region (file "fuzz_crash_032.md") (start 6 26) (end 6 37) (annotation error) (line-text "    match color { RGB => LocalStatus.Pending"))
			(line-break)
			(line-break)
			(annotated emphasis "Hint:")
			(reflow " You can declare this type with ")
			(annotated code ":=")
			(reflow " to make it nominal.")))
	(report
		(severity runtime_error)
		(title "Invalid Pattern")
		(headline
			(reflow "This pattern contains invalid syntax or uses unsupported features."))
		(document))
	(report
		(severity runtime_error)
		(title "Undeclared Type")
		(region (start 8 3) (end 8 4))
		(headline
			(reflow "The type ")
			(annotated code "B")
			(reflow " is not declared in this scope."))
		(document
			(source-region (file "fuzz_crash_032.md") (start 8 3) (end 8 4) (annotation error) (line-text "  B.Blue => LocalStatus.Pending"))))
	(report
		(severity runtime_error)
		(title "Expected Nominal Type")
		(region (start 8 13) (end 8 24))
		(headline
			(reflow "You are using the type ")
			(annotated code "LocalStatus")
			(reflow " like a nominal type, but it is an alias."))
		(document
			(source-region (file "fuzz_crash_032.md") (start 8 13) (end 8 24) (annotation error) (line-text "  B.Blue => LocalStatus.Pending"))
			(line-break)
			(line-break)
			(annotated emphasis "Hint:")
			(reflow " You can declare this type with ")
			(annotated code ":=")
			(reflow " to make it nominal.")))
	(report
		(severity runtime_error)
		(title "Type Mismatch")
		(region (start 7 10) (end 7 21))
		(headline
			(reflow "The")
			(reflow " ")
			(reflow "second")
			(reflow " ")
			(reflow "branch of this")
			(reflow " ")
			(annotated code "match")
			(reflow " ")
			(reflow "does not match the previous")
			(reflow " ")
			(reflow "branches")
			(reflow " ")
			(reflow "."))
		(document
			(source-region (file "fuzz_crash_032.md") (start 7 10) (end 7 21) (annotation error) (line-text "Green => LocalStatus-Complete"))
			(line-break)
			(reflow "The")
			(reflow " ")
			(reflow "second")
			(reflow " ")
			(reflow "branch is:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "[LocalStatus, ..]")
			(annotation-end)
			(line-break)
			(line-break)
			(reflow "But the previous")
			(reflow " ")
			(reflow "branches result")
			(reflow " ")
			(reflow "in:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "tus")
			(annotation-end)
			(line-break)
			(line-break)
			(reflow "All branches in a")
			(reflow " ")
			(annotated code "match")
			(reflow " ")
			(reflow "must have compatible types.")
			(line-break)
			(annotated underline "Note:")
			(reflow " ")
			(reflow "You can wrap branches values in a tag to make them compatible.")
			(line-break)
			(reflow "To learn about tags, see")
			(reflow " ")
			(link "https://www.roc-lang.org/tutorial#tags"))))
~~~
# TOKENS
~~~zig
UpperIdent,OpColon,LowerIdent,OpFatArrow,UpperIdent,OpAssign,OpenSquare,UpperIdent,Comma,UpperIdent,CloseSquare,
LowerIdent,OpColon,Underscore,OpArrow,LowerIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenCurly,KwImport,UpperIdent,NoSpaceDotUpperIdent,
KwMatch,LowerIdent,OpenCurly,UpperIdent,OpFatArrow,UpperIdent,NoSpaceDotUpperIdent,
UpperIdent,OpFatArrow,UpperIdent,OpUnaryMinus,UpperIdent,
UpperIdent,NoSpaceDotUpperIdent,OpFatArrow,UpperIdent,NoSpaceDotUpperIdent,
CloseCurly,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-type-decl
			(header (name "LocalStatus")
				(args))
			(ty-fn
				(ty-var (raw "lue"))
				(ty (name "Loc"))))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "expected_colon_after_type_annotation"))
		(s-malformed (tag "expected_colon_after_type_annotation"))
		(s-type-anno (name "olor")
			(ty-fn
				(_)
				(ty-var (raw "tus"))))
		(s-decl
			(p-ident (raw "olor"))
			(e-lambda
				(args
					(p-ident (raw "color")))
				(e-block
					(statements
						(s-malformed (tag "import_must_be_top_level"))
						(e-tag (raw "Color.RGB"))
						(e-match
							(e-ident (raw "color"))
							(branches
								(branch
									(p-tag (raw "RGB"))
									(e-tag (raw "LocalStatus.Pending")))
								(branch
									(p-tag (raw "Green"))
									(e-tag (raw "LocalStatus")))
								(branch
									(p-malformed (tag "pattern_unexpected_token"))
									(e-tag (raw "Complete")))
								(branch
									(p-tag (raw ".Blue"))
									(e-tag (raw "LocalStatus.Pending")))))))))))
~~~
# FORMATTED
~~~roc
LocalStatus : lue => Loc


olor : _ -> tus
olor = |color| {
		Color.RGB

	match color {
		RGB => LocalStatus.Pending
		Green => LocalStatus
		 => Complete
		B.Blue => LocalStatus.Pending
	}
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "olor"))
		(e-runtime-error (tag "erroneous_value_expr"))
		(annotation
			(ty-fn (effectful false)
				(ty-underscore)
				(ty-rigid-var (name "tus")))))
	(s-alias-decl
		(ty-header (name "LocalStatus"))
		(ty-fn (effectful true)
			(ty-malformed)
			(ty-malformed))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "_arg -> tus")))
	(type_decls
		(alias (type "Error")
			(ty-header (name "LocalStatus"))))
	(expressions
		(expr (type "_arg -> tus"))))
~~~
