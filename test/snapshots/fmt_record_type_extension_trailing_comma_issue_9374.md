# META
~~~ini
description=Issue 9374 - Formatter must not crash with integer underflow on a record type whose extension has a trailing comma
type=snippet
~~~
# SOURCE
~~~roc
x : { ..a, }
~~~
# EXPECTED
DECLARATION HAS NO VALUE - fmt_record_type_extension_trailing_comma_issue_9374.md:1:1:1:13
# PROBLEMS
~~~clojure
(reports
	(report
		(severity warning)
		(title "Declaration Has No Value")
		(region (start 1 1) (end 1 13))
		(headline
			(reflow "This declaration has a type annotation but no implementation."))
		(document
			(source-region (file "fmt_record_type_extension_trailing_comma_issue_9374.md") (start 1 1) (end 1 13) (annotation error) (line-text "x : { ..a, }"))
			(line-break)
			(line-break)
			(reflow "Add a value body here, or put hosted functions in a platform type mod so they are published through the host boundary."))))
~~~
# TOKENS
~~~zig
LowerIdent,OpColon,OpenCurly,DoubleDot,LowerIdent,Comma,CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-type-anno (name "x")
			(ty-record
				(ty-record-ext
					(ty-var (raw "a")))))))
~~~
# FORMATTED
~~~roc
x : {
	..a,
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "x"))
		(e-anno-only)
		(annotation
			(ty-record))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "{ ..a }")))
	(expressions
		(expr (type "{ ..a }"))))
~~~
