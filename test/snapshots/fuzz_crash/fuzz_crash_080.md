# META
~~~ini
description=fuzz crash
type=snippet
~~~
# SOURCE
~~~roc
c : L
        where [
                o
                .h : a,
        ]
~~~
# EXPECTED
UNDECLARED TYPE - fuzz_crash_080.md:1:5:1:6
UNBOUND WHERE RECEIVER - fuzz_crash_080.md:3:17:4:23
DECLARATION HAS NO VALUE - fuzz_crash_080.md:1:1:5:10
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Undeclared Type")
		(region (start 1 5) (end 1 6))
		(headline
			(reflow "The type ")
			(annotated code "L")
			(reflow " is not declared in this scope."))
		(document
			(source-region (file "fuzz_crash_080.md") (start 1 5) (end 1 6) (annotation error) (line-text "c : L"))))
	(report
		(severity runtime_error)
		(title "Unbound Where Receiver")
		(region (start 3 17) (end 4 23))
		(headline
			(reflow "The type variable")
			(reflow " ")
			(annotated code "o")
			(reflow " ")
			(reflow "is not introduced by this annotation's type or a connected method constraint, so this where clause cannot add the")
			(reflow " ")
			(annotated symbol "h")
			(reflow " ")
			(reflow "method to it."))
		(document
			(source-region (file "fuzz_crash_080.md") (start 3 17) (end 4 23) (annotation error) (line-text "                o\n                .h : a,"))
			(line-break)
			(reflow "A where clause receiver must be introduced by the annotation's type, or by the method type of a receiver that is already connected to the annotation. Connect")
			(reflow " ")
			(annotated code "o")
			(reflow " ")
			(reflow "to the annotation, or remove this constraint.")))
	(report
		(severity warning)
		(title "Declaration Has No Value")
		(region (start 1 1) (end 5 10))
		(headline
			(reflow "This declaration has a type annotation but no implementation."))
		(document
			(source-region (file "fuzz_crash_080.md") (start 1 1) (end 5 10) (annotation error) (line-text "c : L\n        where [\n                o\n                .h : a,\n        ]"))
			(line-break)
			(line-break)
			(reflow "Add a value body here, or put hosted functions in a platform type mod so they are published through the host boundary."))))
~~~
# TOKENS
~~~zig
LowerIdent,OpColon,UpperIdent,
KwWhere,OpenSquare,
LowerIdent,
DotLowerIdent,OpColon,LowerIdent,Comma,
CloseSquare,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-type-anno (name "c")
			(ty (name "L"))
			(where
				(method (mod-of "o") (name "h")
					(args)
					(ty-var (raw "a")))))))
~~~
# FORMATTED
~~~roc
c : L
	where [
		o
		.h : a,
	]
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "c"))
		(e-runtime-error (tag "erroneous_value_expr"))
		(annotation
			(ty-malformed)
			(where
				(method (ty-rigid-var (name "o")) (name "h")
					(args)
					(ty-rigid-var (name "a")))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Error")))
	(expressions
		(expr (type "Error"))))
~~~
