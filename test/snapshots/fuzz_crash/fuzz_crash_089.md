# META
~~~ini
description=Issue #10097: Unstable formatting in parser/formatter roundtrip
type=file
~~~
# SOURCE
~~~roc
e={0#
.{}}
~~~
# EXPECTED
UNRECOGNIZED SYNTAX - fuzz_crash_089.md:1:4:2:4
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Unrecognized Syntax")
		(region (start 1 4) (end 2 4))
		(headline
			(reflow "I don't recognize this syntax."))
		(document
			(source-region (file "fuzz_crash_089.md") (start 1 4) (end 2 4) (annotation error) (line-text "e={0#\n.{}}"))
			(line-break)
			(reflow "This might be a syntax error, an unsupported language feature, or a typo."))))
~~~
# TOKENS
~~~zig
LowerIdent,OpAssign,OpenCurly,Int,
Dot,OpenCurly,CloseCurly,CloseCurly,
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
					(e-nominal-record
						(mapper (e-int (raw "0")))
						(backing (e-record))))))))
~~~
# FORMATTED
~~~roc
e = {
	0 #
	.{}
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
