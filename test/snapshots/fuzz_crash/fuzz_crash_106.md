# META
~~~ini
description=parser crash: formatter output no longer parses after reformat
type=file
source_escapes=true
~~~
# SOURCE
~~~roc
a=0O0\r.0
~~~
# EXPECTED
UPPERCASE BASE - :0:0:0:0
MISPLACED CARRIAGE RETURN - :0:0:0:0
INVALID TUPLE ACCESS - fuzz_crash_106.md:1:3:1:9
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Uppercase Base")
		(headline
			(reflow "Number base prefixes must be lowercase (0x, 0o, 0b)."))
		(document))
	(report
		(severity runtime_error)
		(title "Misplaced Carriage Return")
		(headline
			(reflow "Carriage return characters (\\r) are not allowed in Roc source code."))
		(document))
	(report
		(severity runtime_error)
		(title "Invalid Tuple Access")
		(region (start 1 3) (end 1 9))
		(headline
			(reflow "This value is not a tuple, so it has no .0 element."))
		(document
			(source-region (file "fuzz_crash_106.md") (start 1 3) (end 1 9) (annotation error) (line-text "a=0O0\r.0")))))
~~~
# TOKENS
~~~zig
LowerIdent,OpAssign,Int,DotInt,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-decl
			(p-ident (raw "a"))
			(e-tuple-access
				(e-int (raw "0O0"))
				".0"))))
~~~
# FORMATTED
~~~roc
a = (0O0).0
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "a"))
		(e-tuple-access (index "0")
			(e-num (value "0")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Error")))
	(expressions
		(expr (type "Error"))))
~~~
