# META
~~~ini
description=fuzz crash: formatter parser output instability
type=snippet
~~~
# SOURCE
~~~roc
i:U
d:i
o=||{D()=0}
()=()
~~~
# EXPECTED
UNDECLARED TYPE - fuzz_crash_082.md:1:3:1:4
EMPTY TUPLE NOT ALLOWED - fuzz_crash_082.md:4:4:4:6
DECLARATION HAS NO VALUE - fuzz_crash_082.md:1:1:1:4
DECLARATION HAS NO VALUE - fuzz_crash_082.md:2:1:2:4
MISSING METHOD - fuzz_crash_082.md:3:10:3:11
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Undeclared Type")
		(region (start 1 3) (end 1 4))
		(headline
			(reflow "The type ")
			(annotated code "U")
			(reflow " is not declared in this scope."))
		(document
			(source-region (file "fuzz_crash_082.md") (start 1 3) (end 1 4) (annotation error) (line-text "i:U"))))
	(report
		(severity runtime_error)
		(title "Empty Tuple Not Allowed")
		(region (start 4 4) (end 4 6))
		(headline
			(reflow "I am part way through parsing this tuple, but it is empty."))
		(document
			(source-region (file "fuzz_crash_082.md") (start 4 4) (end 4 6) (annotation error) (line-text "()=()"))
			(line-break)
			(reflow "If you want to represent nothing, try using an empty record: ")
			(annotated code "{}")
			(reflow ".")))
	(report
		(severity warning)
		(title "Declaration Has No Value")
		(region (start 1 1) (end 1 4))
		(headline
			(reflow "This declaration has a type annotation but no implementation."))
		(document
			(source-region (file "fuzz_crash_082.md") (start 1 1) (end 1 4) (annotation error) (line-text "i:U"))
			(line-break)
			(line-break)
			(reflow "Add a value body here, or put hosted functions in a platform type mod so they are published through the host boundary.")))
	(report
		(severity warning)
		(title "Declaration Has No Value")
		(region (start 2 1) (end 2 4))
		(headline
			(reflow "This declaration has a type annotation but no implementation."))
		(document
			(source-region (file "fuzz_crash_082.md") (start 2 1) (end 2 4) (annotation error) (line-text "d:i"))
			(line-break)
			(line-break)
			(reflow "Add a value body here, or put hosted functions in a platform type mod so they are published through the host boundary.")))
	(report
		(severity runtime_error)
		(title "Missing Method")
		(region (start 3 10) (end 3 11))
		(headline
			(reflow "This")
			(reflow " ")
			(annotated code "from_numeral")
			(reflow " ")
			(reflow "method is being called on a value whose type doesn't have that method."))
		(document
			(source-region (file "fuzz_crash_082.md") (start 3 10) (end 3 11) (annotation error) (line-text "o=||{D()=0}"))
			(line-break)
			(reflow "The value's type, which does not have a method named ")
			(annotated code "from_numeral")
			(reflow ",")
			(reflow " ")
			(reflow "is:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "[D]")
			(annotation-end))))
~~~
# TOKENS
~~~zig
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpColon,LowerIdent,
LowerIdent,OpAssign,OpBar,OpBar,OpenCurly,UpperIdent,NoSpaceOpenRound,CloseRound,OpAssign,Int,CloseCurly,
OpenRound,CloseRound,OpAssign,NoSpaceOpenRound,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-type-anno (name "i")
			(ty (name "U")))
		(s-type-anno (name "d")
			(ty-var (raw "i")))
		(s-decl
			(p-ident (raw "o"))
			(e-lambda
				(args)
				(e-block
					(statements
						(s-decl
							(p-tag (raw "D"))
							(e-int (raw "0")))))))
		(s-decl
			(p-tuple)
			(e-tuple))))
~~~
# FORMATTED
~~~roc
i : U

d : i

o = || {
	D() = 0
}

() = ()
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "i"))
		(e-runtime-error (tag "erroneous_value_expr"))
		(annotation
			(ty-malformed)))
	(d-let
		(p-assign (ident "d"))
		(e-anno-only)
		(annotation
			(ty-rigid-var (name "i"))))
	(d-let
		(p-assign (ident "o"))
		(e-lambda
			(args)
			(e-block
				(s-let
					(p-applied-tag)
					(e-runtime-error (tag "erroneous_value_expr")))
				(e-empty_record))))
	(d-let
		(p-tuple
			(patterns))
		(e-runtime-error (tag "empty_tuple"))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Error"))
		(patt (type "i"))
		(patt (type "({}) -> {}")))
	(expressions
		(expr (type "Error"))
		(expr (type "i"))
		(expr (type "({}) -> {}"))
		(expr (type "Error"))))
~~~
