# META
~~~ini
description=formatter instability with leading newline
type=file
~~~
# SOURCE
~~~roc

b:r
~~~
# EXPECTED
DECLARATION HAS NO VALUE - fuzz_crash_079.md:2:1:2:4
# PROBLEMS
~~~clojure
(reports
	(report
		(severity warning)
		(title "Declaration Has No Value")
		(region (start 2 1) (end 2 4))
		(headline
			(reflow "This declaration has a type annotation but no implementation."))
		(document
			(source-region (file "fuzz_crash_079.md") (start 2 1) (end 2 4) (annotation error) (line-text "b:r"))
			(line-break)
			(line-break)
			(reflow "Add a value body here, or put hosted functions in a platform type mod so they are published through the host boundary."))))
~~~
# TOKENS
~~~zig
LowerIdent,OpColon,LowerIdent,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-type-anno (name "b")
			(ty-var (raw "r")))))
~~~
# FORMATTED
~~~roc

b : r
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "b"))
		(e-anno-only)
		(annotation
			(ty-rigid-var (name "r")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "r")))
	(expressions
		(expr (type "r"))))
~~~
