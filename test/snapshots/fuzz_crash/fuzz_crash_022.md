# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
app [main!] { |f: platform "c" }

UserId : U64

ser : UserId -> Str
getUser = |id| if (id > 1!) "big" else "l"

-ain! = |_| getUser(900)
~~~
# EXPECTED
EXPECTED DEPENDENCY NAME - fuzz_crash_022.md:1:1:1:4
UNEXPECTED TYPE SYNTAX - fuzz_crash_022.md:1:19:1:27
UNEXPECTED STATEMENT - fuzz_crash_022.md:1:28:1:29
UNEXPECTED STATEMENT - fuzz_crash_022.md:1:29:1:30
UNEXPECTED STATEMENT - fuzz_crash_022.md:1:30:1:31
UNEXPECTED STATEMENT - fuzz_crash_022.md:1:32:1:33
UNEXPECTED EXPRESSION SYNTAX - fuzz_crash_022.md:6:27:6:28
UNEXPECTED EXPRESSION SYNTAX - fuzz_crash_022.md:6:35:6:39
UNEXPECTED EXPRESSION SYNTAX - fuzz_crash_022.md:8:7:8:8
EXPECTED TUPLE SEPARATOR - fuzz_crash_022.md:9:1:9:1
UNEXPECTED EXPRESSION SYNTAX - fuzz_crash_022.md:9:1:9:1
MALFORMED TYPE - fuzz_crash_022.md:1:19:1:27
INVALID IF CONDITION - :0:0:0:0
UNUSED VARIABLE - fuzz_crash_022.md:6:12:6:14
DECLARATION HAS NO VALUE - fuzz_crash_022.md:1:16:1:27
DECLARATION HAS NO VALUE - fuzz_crash_022.md:5:1:5:20
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Expected Dependency Name")
		(region (start 1 1) (end 1 4))
		(headline
			(reflow "I was parsing an app dependency record, and I expected a lowercase field name."))
		(document
			(reflow "Each package or platform entry starts with a lowercase field name, followed by ")
			(annotated code ":")
			(reflow " and a string path or ")
			(annotated code "platform")
			(reflow " path.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "pf: platform \"../platform/main.roc\"")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "app")
			(text " here.")
			(line-break)
			(reflow "That word is reserved by Roc, so it cannot be used as a name in this position.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_022.md") (start 1 1) (end 1 4) (annotation error) (line-text "app [main!] { |f: platform \"c\" }"))))
	(report
		(severity runtime_error)
		(title "Unexpected Type Syntax")
		(region (start 1 19) (end 1 27))
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
			(annotated code "platform")
			(text " here.")
			(line-break)
			(reflow "That word is reserved by Roc, so it cannot be used as a name in this position.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_022.md") (start 1 19) (end 1 27) (annotation error) (line-text "app [main!] { |f: platform \"c\" }"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 1 28) (end 1 29))
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
			(source-region (file "fuzz_crash_022.md") (start 1 28) (end 1 29) (annotation error) (line-text "app [main!] { |f: platform \"c\" }"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 1 29) (end 1 30))
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
			(annotated code "c")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_022.md") (start 1 29) (end 1 30) (annotation error) (line-text "app [main!] { |f: platform \"c\" }"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 1 30) (end 1 31))
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
			(source-region (file "fuzz_crash_022.md") (start 1 30) (end 1 31) (annotation error) (line-text "app [main!] { |f: platform \"c\" }"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 1 32) (end 1 33))
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
			(source-region (file "fuzz_crash_022.md") (start 1 32) (end 1 33) (annotation error) (line-text "app [main!] { |f: platform \"c\" }"))))
	(report
		(severity runtime_error)
		(title "Unexpected Expression Syntax")
		(region (start 6 27) (end 6 28))
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
			(source-region (file "fuzz_crash_022.md") (start 6 27) (end 6 28) (annotation error) (line-text "getUser = |id| if (id > 1!) \"big\" else \"l\""))))
	(report
		(severity runtime_error)
		(title "Unexpected Expression Syntax")
		(region (start 6 35) (end 6 39))
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
			(annotated code "else")
			(text " here.")
			(line-break)
			(reflow "That word is reserved by Roc, so it cannot be used as a name in this position.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_022.md") (start 6 35) (end 6 39) (annotation error) (line-text "getUser = |id| if (id > 1!) \"big\" else \"l\""))))
	(report
		(severity runtime_error)
		(title "Unexpected Expression Syntax")
		(region (start 8 7) (end 8 8))
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
			(annotated code "=")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_022.md") (start 8 7) (end 8 8) (annotation error) (line-text "-ain! = |_| getUser(900)"))))
	(report
		(severity runtime_error)
		(title "Expected Tuple Separator")
		(region (start 9 1) (end 9 1))
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
			(source-region (file "fuzz_crash_022.md") (start 9 1) (end 9 1) (annotation error) (line-text ""))))
	(report
		(severity runtime_error)
		(title "Unexpected Expression Syntax")
		(region (start 9 1) (end 9 1))
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
			(reflow "I reached the end of the file before this construct was complete.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_022.md") (start 9 1) (end 9 1) (annotation error) (line-text ""))))
	(report
		(severity runtime_error)
		(title "Malformed Type")
		(region (start 1 19) (end 1 27))
		(headline
			(reflow "This type annotation is malformed or contains invalid syntax."))
		(document
			(source-region (file "fuzz_crash_022.md") (start 1 19) (end 1 27) (annotation error) (line-text "app [main!] { |f: platform \"c\" }"))))
	(report
		(severity runtime_error)
		(title "Invalid If Condition")
		(headline
			(reflow "The condition in this ")
			(annotated keyword "if")
			(reflow " expression could not be processed."))
		(document
			(reflow "The condition must be a valid expression that evaluates to a ")
			(annotated keyword "Bool")
			(reflow " value (")
			(annotated keyword "Bool.true")
			(reflow " or ")
			(annotated keyword "Bool.false")
			(reflow ").")))
	(report
		(severity warning)
		(title "Unused Variable")
		(region (start 6 12) (end 6 14))
		(headline
			(reflow "Variable ")
			(annotated symbol-unqualified "id")
			(reflow " is defined here and then never used:"))
		(document
			(reflow "If you don't need this variable, prefix it with an underscore like ")
			(annotated symbol-unqualified "_id")
			(reflow " to suppress this warning.")
			(line-break)
			(source-region (file "fuzz_crash_022.md") (start 6 12) (end 6 14) (annotation error) (line-text "getUser = |id| if (id > 1!) \"big\" else \"l\""))))
	(report
		(severity warning)
		(title "Declaration Has No Value")
		(region (start 1 16) (end 1 27))
		(headline
			(reflow "This declaration has a type annotation but no implementation."))
		(document
			(source-region (file "fuzz_crash_022.md") (start 1 16) (end 1 27) (annotation error) (line-text "app [main!] { |f: platform \"c\" }"))
			(line-break)
			(line-break)
			(reflow "Add a value body here, or put hosted functions in a platform type mod so they are published through the host boundary.")))
	(report
		(severity warning)
		(title "Declaration Has No Value")
		(region (start 5 1) (end 5 20))
		(headline
			(reflow "This declaration has a type annotation but no implementation."))
		(document
			(source-region (file "fuzz_crash_022.md") (start 5 1) (end 5 20) (annotation error) (line-text "ser : UserId -> Str"))
			(line-break)
			(line-break)
			(reflow "Add a value body here, or put hosted functions in a platform type mod so they are published through the host boundary."))))
~~~
# TOKENS
~~~zig
KwApp,OpenSquare,LowerIdent,CloseSquare,OpenCurly,OpBar,LowerIdent,OpColon,KwPlatform,StringStart,StringPart,StringEnd,CloseCurly,
UpperIdent,OpColon,UpperIdent,
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,KwIf,OpenRound,LowerIdent,OpGreaterThan,Int,OpBang,CloseRound,StringStart,StringPart,StringEnd,KwElse,StringStart,StringPart,StringEnd,
OpUnaryMinus,LowerIdent,OpAssign,OpBar,Underscore,OpBar,LowerIdent,NoSpaceOpenRound,Int,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(malformed-header (tag "expected_package_or_platform_name"))
	(statements
		(s-type-anno (name "f")
			(ty-malformed (tag "ty_anno_unexpected_token")))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-type-decl
			(header (name "UserId")
				(args))
			(ty (name "U64")))
		(s-type-anno (name "ser")
			(ty-fn
				(ty (name "UserId"))
				(ty (name "Str"))))
		(s-decl
			(p-ident (raw "getUser"))
			(e-lambda
				(args
					(p-ident (raw "id")))
				(e-if-without-else
					(e-malformed (reason "expected_expr_close_round_or_comma"))
					(e-malformed (reason "expr_unexpected_token")))))))
~~~
# FORMATTED
~~~roc
f :


UserId : U64

ser : UserId -> Str

getUser = |id| if
	
	~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "f"))
		(e-runtime-error (tag "erroneous_value_expr"))
		(annotation
			(ty-malformed)))
	(d-let
		(p-assign (ident "ser"))
		(e-anno-only)
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "UserId") (local))
				(ty-lookup (name "Str") (builtin)))))
	(d-let
		(p-assign (ident "getUser"))
		(e-runtime-error (tag "erroneous_value_expr")))
	(s-alias-decl
		(ty-header (name "UserId"))
		(ty-lookup (name "U64") (builtin))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Error"))
		(patt (type "UserId -> Str"))
		(patt (type "_arg -> Error")))
	(type_decls
		(alias (type "UserId")
			(ty-header (name "UserId"))))
	(expressions
		(expr (type "Error"))
		(expr (type "UserId -> Str"))
		(expr (type "_arg -> Error"))))
~~~
