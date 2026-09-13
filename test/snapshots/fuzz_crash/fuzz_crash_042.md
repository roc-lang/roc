# META
~~~ini
description=fuzz crash
type=snippet
~~~
# SOURCE
~~~roc
import u.R}g:r->R.a.E
~~~
# EXPECTED
UNEXPECTED STATEMENT - fuzz_crash_042.md:1:11:1:12
MOD NOT FOUND - fuzz_crash_042.md:1:20:1:22
DECLARATION HAS NO VALUE - fuzz_crash_042.md:1:12:1:22
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 1 11) (end 1 12))
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
			(source-region (file "fuzz_crash_042.md") (start 1 11) (end 1 12) (annotation error) (line-text "import u.R}g:r->R.a.E"))))
	(report
		(severity runtime_error)
		(title "Mod Not Found")
		(region (start 1 20) (end 1 22))
		(headline
			(text "This ")
			(annotated code "a.E")
			(reflow " type is declared to be in ")
			(annotated code "u.R")
			(reflow ", which does not exist."))
		(document
			(source-region (file "fuzz_crash_042.md") (start 1 20) (end 1 22) (annotation error) (line-text "import u.R}g:r->R.a.E"))))
	(report
		(severity warning)
		(title "Declaration Has No Value")
		(region (start 1 12) (end 1 22))
		(headline
			(reflow "This declaration has a type annotation but no implementation."))
		(document
			(source-region (file "fuzz_crash_042.md") (start 1 12) (end 1 22) (annotation error) (line-text "import u.R}g:r->R.a.E"))
			(line-break)
			(line-break)
			(reflow "Add a value body here, or put hosted functions in a platform type mod so they are published through the host boundary."))))
~~~
# TOKENS
~~~zig
KwImport,LowerIdent,NoSpaceDotUpperIdent,CloseCurly,LowerIdent,OpColon,LowerIdent,OpArrow,UpperIdent,NoSpaceDotLowerIdent,NoSpaceDotUpperIdent,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-import (raw "u.R"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-type-anno (name "g")
			(ty-fn
				(ty-var (raw "r"))
				(ty (name "R.a.E"))))))
~~~
# FORMATTED
~~~roc
import u.R
g : r -> R.a.E
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "g"))
		(e-runtime-error (tag "erroneous_value_expr"))
		(annotation
			(ty-fn (effectful false)
				(ty-rigid-var (name "r"))
				(ty-malformed))))
	(s-import (mod "u.R")
		(exposes)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "r -> Error")))
	(expressions
		(expr (type "r -> Error"))))
~~~
