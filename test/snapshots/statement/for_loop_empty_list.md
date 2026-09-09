# META
~~~ini
description=For loop with empty list
type=snippet
~~~
# SOURCE
~~~roc
unchanged : U64
unchanged = {
	var value_ = 42
	for n in [] {
		value_ = n
	}
	value_
}

expect unchanged == 42
~~~
# EXPECTED
VAR NAME MISSING `$` - for_loop_empty_list.md:3:6:3:12
# PROBLEMS
~~~clojure
(reports
	(report
		(severity warning)
		(title "Var Name Missing `$`")
		(region (start 3 6) (end 3 12))
		(headline
			(reflow "The mutable binding ")
			(annotated symbol-unqualified "value_")
			(reflow " is declared with ")
			(annotated keyword "var")
			(reflow " but its name does not start with ")
			(annotated code "$")
			(reflow "."))
		(document
			(reflow "Rename this binding and all of its uses to ")
			(annotated symbol-unqualified "$value_")
			(reflow ". The name is only a convention; mutability comes from the ")
			(annotated keyword "var")
			(reflow " declaration.")
			(line-break)
			(line-break)
			(source-region (file "for_loop_empty_list.md") (start 3 6) (end 3 12) (annotation warning) (line-text "\tvar value_ = 42")))))
~~~
# TOKENS
~~~zig
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,OpenCurly,
KwVar,LowerIdent,OpAssign,Int,
KwFor,LowerIdent,KwIn,OpenSquare,CloseSquare,OpenCurly,
LowerIdent,OpAssign,LowerIdent,
CloseCurly,
LowerIdent,
CloseCurly,
KwExpect,LowerIdent,OpEquals,Int,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-type-anno (name "unchanged")
			(ty (name "U64")))
		(s-decl
			(p-ident (raw "unchanged"))
			(e-block
				(statements
					(s-var (name "value_")
						(e-int (raw "42")))
					(s-for
						(p-ident (raw "n"))
						(e-list)
						(e-block
							(statements
								(s-decl
									(p-ident (raw "value_"))
									(e-ident (raw "n"))))))
					(e-ident (raw "value_")))))
		(s-expect
			(e-binop (op "==")
				(e-ident (raw "unchanged"))
				(e-int (raw "42"))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "unchanged"))
		(e-block
			(s-var
				(p-var-assign (ident "value_"))
				(e-num (value "42")))
			(s-for
				(p-assign (ident "n"))
				(e-empty_list)
				(e-block
					(s-reassign
						(p-var-assign (ident "value_"))
						(e-lookup-local
							(p-assign (ident "n"))))
					(e-empty_record)))
			(e-lookup-local
				(p-var-assign (ident "value_"))))
		(annotation
			(ty-lookup (name "U64") (builtin))))
	(s-expect
		(e-method-eq (negated "false")
			(lhs
				(e-lookup-local
					(p-assign (ident "unchanged"))))
			(rhs
				(e-num (value "42"))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "U64")))
	(expressions
		(expr (type "U64"))))
~~~
