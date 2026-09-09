# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
platform # Cd
	"foo" # Ce
	requires # Co		{	} #ose
			{ # d
			n! : List(Str) => {}, # ure
			} #Ce
	exposes #rd
		[ #
		] # Cse
	packages # Cd
		vides # Cd
		{ # pen
pkg: "..l", mmen		} # Cose
	provides # Cd
		[ Ok(world), (n # pen
ar,
		]
~~~
# EXPECTED
EXPECTED OPENING BRACE - fuzz_crash_029.md:11:3:11:8
EXPECTED PROVIDES - fuzz_crash_029.md:12:3:12:4
UNEXPECTED TYPE SYNTAX - fuzz_crash_029.md:13:6:13:7
UNEXPECTED STATEMENT - fuzz_crash_029.md:13:7:13:10
UNEXPECTED STATEMENT - fuzz_crash_029.md:13:10:13:11
UNEXPECTED STATEMENT - fuzz_crash_029.md:13:11:13:12
UNEXPECTED STATEMENT - fuzz_crash_029.md:13:13:13:17
UNEXPECTED STATEMENT - fuzz_crash_029.md:13:19:13:20
UNEXPECTED STATEMENT - fuzz_crash_029.md:14:2:14:10
UNEXPECTED STATEMENT - fuzz_crash_029.md:15:3:15:4
TYPE APPLICATION NEEDS PARENTHESES - fuzz_crash_029.md:15:14:15:15
UNEXPECTED STATEMENT - fuzz_crash_029.md:15:16:15:17
UNEXPECTED STATEMENT - fuzz_crash_029.md:15:17:15:18
UNEXPECTED STATEMENT - fuzz_crash_029.md:16:1:16:3
UNEXPECTED STATEMENT - fuzz_crash_029.md:16:3:16:4
UNEXPECTED STATEMENT - fuzz_crash_029.md:17:3:17:4
MALFORMED TYPE - fuzz_crash_029.md:13:6:13:7
DECLARATION HAS NO VALUE - fuzz_crash_029.md:13:1:13:7
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Expected Opening Brace")
		(region (start 11 3) (end 11 8))
		(headline
			(reflow "I was parsing a `packages` section, and I expected an opening `{`."))
		(document
			(reflow "Package dependencies are written as record fields inside braces.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "packages { base: \"../base/main.roc\" }")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "vides")
			(text " here.")
			(line-break)
			(reflow "Names that start with lowercase letters are value names or record field names, depending on the surrounding syntax.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_029.md") (start 11 3) (end 11 8) (annotation error) (line-text "\t\tvides # Cd"))))
	(report
		(severity runtime_error)
		(title "Expected Provides")
		(region (start 12 3) (end 12 4))
		(headline
			(reflow "I was parsing a platform header, and I expected the `provides` section."))
		(document
			(reflow "A platform header must map host symbols to Roc functions in a ")
			(annotated code "provides")
			(reflow " record.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "provides { \"roc_main\": main }")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "{")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_029.md") (start 12 3) (end 12 4) (annotation error) (line-text "\t\t{ # pen"))))
	(report
		(severity runtime_error)
		(title "Unexpected Type Syntax")
		(region (start 13 6) (end 13 7))
		(headline
			(reflow "I was parsing a type annotation, and this token cannot start a type here."))
		(document
			(reflow "Types can be type variables, uppercase type names, function types, tuples, records, or tag unions.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "List(U64)")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "\"")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_029.md") (start 13 6) (end 13 7) (annotation error) (line-text "pkg: \"..l\", mmen\t\t} # Cose"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 13 7) (end 13 10))
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
			(annotated code "..l")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_029.md") (start 13 7) (end 13 10) (annotation error) (line-text "pkg: \"..l\", mmen\t\t} # Cose"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 13 10) (end 13 11))
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
			(annotated code "\"")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_029.md") (start 13 10) (end 13 11) (annotation error) (line-text "pkg: \"..l\", mmen\t\t} # Cose"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 13 11) (end 13 12))
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
			(annotated code ",")
			(text " here.")
			(line-break)
			(reflow "A comma separates items, but there must be a valid item on both sides of it.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_029.md") (start 13 11) (end 13 12) (annotation error) (line-text "pkg: \"..l\", mmen\t\t} # Cose"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 13 13) (end 13 17))
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
			(annotated code "mmen")
			(text " here.")
			(line-break)
			(reflow "Names that start with lowercase letters are value names or record field names, depending on the surrounding syntax.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_029.md") (start 13 13) (end 13 17) (annotation error) (line-text "pkg: \"..l\", mmen\t\t} # Cose"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 13 19) (end 13 20))
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
			(annotated code "}")
			(text " here.")
			(line-break)
			(reflow "This closes the current construct, so the parser was looking for the missing item before it.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_029.md") (start 13 19) (end 13 20) (annotation error) (line-text "pkg: \"..l\", mmen\t\t} # Cose"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 14 2) (end 14 10))
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
			(annotated code "provides")
			(text " here.")
			(line-break)
			(reflow "That word is reserved by Roc, so it cannot be used as a name in this position.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_029.md") (start 14 2) (end 14 10) (annotation error) (line-text "\tprovides # Cd"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 15 3) (end 15 4))
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
			(source-region (file "fuzz_crash_029.md") (start 15 3) (end 15 4) (annotation error) (line-text "\t\t[ Ok(world), (n # pen"))))
	(report
		(severity runtime_error)
		(title "Type Application Needs Parentheses")
		(region (start 15 14) (end 15 15))
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
			(source-region (file "fuzz_crash_029.md") (start 15 14) (end 15 15) (annotation error) (line-text "\t\t[ Ok(world), (n # pen"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 15 16) (end 15 17))
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
			(annotated code "(")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_029.md") (start 15 16) (end 15 17) (annotation error) (line-text "\t\t[ Ok(world), (n # pen"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 15 17) (end 15 18))
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
			(annotated code "n")
			(text " here.")
			(line-break)
			(reflow "Names that start with lowercase letters are value names or record field names, depending on the surrounding syntax.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_029.md") (start 15 17) (end 15 18) (annotation error) (line-text "\t\t[ Ok(world), (n # pen"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 16 1) (end 16 3))
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
			(annotated code "ar")
			(text " here.")
			(line-break)
			(reflow "Names that start with lowercase letters are value names or record field names, depending on the surrounding syntax.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_029.md") (start 16 1) (end 16 3) (annotation error) (line-text "ar,"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 16 3) (end 16 4))
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
			(annotated code ",")
			(text " here.")
			(line-break)
			(reflow "A comma separates items, but there must be a valid item on both sides of it.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_029.md") (start 16 3) (end 16 4) (annotation error) (line-text "ar,"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 17 3) (end 17 4))
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
			(annotated code "]")
			(text " here.")
			(line-break)
			(reflow "This closes the current construct, so the parser was looking for the missing item before it.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_029.md") (start 17 3) (end 17 4) (annotation error) (line-text "\t\t]"))))
	(report
		(severity runtime_error)
		(title "Malformed Type")
		(region (start 13 6) (end 13 7))
		(headline
			(reflow "This type annotation is malformed or contains invalid syntax."))
		(document
			(source-region (file "fuzz_crash_029.md") (start 13 6) (end 13 7) (annotation error) (line-text "pkg: \"..l\", mmen\t\t} # Cose"))))
	(report
		(severity warning)
		(title "Declaration Has No Value")
		(region (start 13 1) (end 13 7))
		(headline
			(reflow "This declaration has a type annotation but no implementation."))
		(document
			(source-region (file "fuzz_crash_029.md") (start 13 1) (end 13 7) (annotation error) (line-text "pkg: \"..l\", mmen\t\t} # Cose"))
			(line-break)
			(line-break)
			(reflow "Add a value body here, or put hosted functions in a platform type mod so they are published through the host boundary."))))
~~~
# TOKENS
~~~zig
KwPlatform,
StringStart,StringPart,StringEnd,
KwRequires,
OpenCurly,
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,OpFatArrow,OpenCurly,CloseCurly,Comma,
CloseCurly,
KwExposes,
OpenSquare,
CloseSquare,
KwPackages,
LowerIdent,
OpenCurly,
LowerIdent,OpColon,StringStart,StringPart,StringEnd,Comma,LowerIdent,CloseCurly,
KwProvides,
OpenSquare,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,Comma,OpenRound,LowerIdent,
LowerIdent,Comma,
CloseSquare,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(malformed-header (tag "expected_provides"))
	(statements
		(s-type-anno (name "pkg")
			(ty-malformed (tag "ty_anno_unexpected_token")))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "expected_colon_after_type_annotation"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))))
~~~
# FORMATTED
~~~roc
# pen
pkg :
# Cose
# Cd
# pen

~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "pkg"))
		(e-runtime-error (tag "erroneous_value_expr"))
		(annotation
			(ty-malformed))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Error")))
	(expressions
		(expr (type "Error"))))
~~~
