# META
~~~ini
description=Float literal type inference
type=snippet
~~~
# SOURCE
~~~roc
x = 3.14

y = 1.23e45

z = 0.5
~~~
# EXPECTED
INVALID NUMBER - can_frac_literal.md:3:5:3:12
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Invalid Number")
		(region (start 3 5) (end 3 12))
		(headline
			(reflow "This number literal does not fit in the inferred type."))
		(document
			(source-region (file "can_frac_literal.md") (start 3 5) (end 3 12) (annotation error) (line-text "y = 1.23e45"))
			(line-break)
			(reflow "The inferred type is:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "Dec")
			(annotation-end))))
~~~
# TOKENS
~~~zig
LowerIdent,OpAssign,Float,
LowerIdent,OpAssign,Float,
LowerIdent,OpAssign,Float,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-decl
			(p-ident (raw "x"))
			(e-frac (raw "3.14")))
		(s-decl
			(p-ident (raw "y"))
			(e-frac (raw "1.23e45")))
		(s-decl
			(p-ident (raw "z"))
			(e-frac (raw "0.5")))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "x"))
		(e-dec-small (numerator "314") (denominator-power-of-ten "2") (value "3.14")))
	(d-let
		(p-assign (ident "y"))
		(e-runtime-error (tag "erroneous_value_expr")))
	(d-let
		(p-assign (ident "z"))
		(e-dec-small (numerator "5") (denominator-power-of-ten "1") (value "0.5"))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Dec"))
		(patt (type "Error"))
		(patt (type "Dec")))
	(expressions
		(expr (type "Dec"))
		(expr (type "Error"))
		(expr (type "Dec"))))
~~~
