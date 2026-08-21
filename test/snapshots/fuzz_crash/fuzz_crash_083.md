# META
~~~ini
description=fuzz regression: type_parameter_conflict diagnostic report
type=file
~~~
# SOURCE
~~~roc
A(a) : a where [a.a1 : (a, a) -> Str]
C(b, b) : (a, b)
D(a, b) : C(a, b)
~~~
# EXPECTED
WHERE CLAUSE NOT ALLOWED IN TYPE DECLARATION - fuzz_crash_083.md:1:1:1:38
TYPE PARAMETER CONFLICT - fuzz_crash_083.md:2:6:2:7
UNDECLARED TYPE VARIABLE - fuzz_crash_083.md:2:12:2:13
TOO MANY ARGS - fuzz_crash_083.md:3:11:3:18
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Where Clause Not Allowed In Type Declaration")
		(region (start 1 1) (end 1 38))
		(headline
			(text "You cannot define a ")
			(annotated code "where")
			(reflow " clause inside a type declaration."))
		(document
			(source-region (file "fuzz_crash_083.md") (start 1 1) (end 1 38) (annotation error) (line-text "A(a) : a where [a.a1 : (a, a) -> Str]"))))
	(report
		(severity runtime_error)
		(title "Type Parameter Conflict")
		(region (start 2 6) (end 2 7))
		(headline
			(text "The type parameter ")
			(annotated symbol-unqualified "b")
			(text " in type ")
			(annotated symbol-unqualified "C")
			(text " conflicts with another declaration."))
		(document
			(reflow "Type parameters must have unique names within their scope.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_083.md") (start 2 6) (end 2 7) (annotation error) (line-text "C(b, b) : (a, b)"))
			(line-break)
			(text "But ")
			(annotated symbol-unqualified "b")
			(text " was already declared in ")
			(source-location
				(file "fuzz_crash_083.md")
				(line 2)
				(column 3))
			(text ":")
			(line-break)
			(source-region (file "fuzz_crash_083.md") (start 2 3) (end 2 4) (annotation dim) (line-text "C(b, b) : (a, b)"))))
	(report
		(severity runtime_error)
		(title "Undeclared Type Variable")
		(region (start 2 12) (end 2 13))
		(headline
			(reflow "The type variable ")
			(annotated code "a")
			(reflow " is not declared in this scope."))
		(document
			(reflow "Type variables must be introduced in a type annotation before they can be used.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_083.md") (start 2 12) (end 2 13) (annotation error) (line-text "C(b, b) : (a, b)"))))
	(report
		(severity runtime_error)
		(title "Too Many Args")
		(region (start 3 11) (end 3 18))
		(headline
			(reflow "The type")
			(reflow " ")
			(annotated type "C")
			(reflow " ")
			(reflow "expects")
			(reflow " ")
			(reflow "1")
			(reflow " ")
			(reflow "argument,")
			(reflow " ")
			(reflow "but got")
			(reflow " ")
			(reflow "2")
			(reflow " ")
			(reflow "instead."))
		(document
			(source-region (file "fuzz_crash_083.md") (start 3 11) (end 3 18) (annotation error) (line-text "D(a, b) : C(a, b)")))))
~~~
# TOKENS
~~~zig
UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpColon,LowerIdent,KwWhere,OpenSquare,LowerIdent,NoSpaceDotLowerIdent,OpColon,OpenRound,LowerIdent,Comma,LowerIdent,CloseRound,OpArrow,UpperIdent,CloseSquare,
UpperIdent,NoSpaceOpenRound,LowerIdent,Comma,LowerIdent,CloseRound,OpColon,OpenRound,LowerIdent,Comma,LowerIdent,CloseRound,
UpperIdent,NoSpaceOpenRound,LowerIdent,Comma,LowerIdent,CloseRound,OpColon,UpperIdent,NoSpaceOpenRound,LowerIdent,Comma,LowerIdent,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-type-decl
			(header (name "A")
				(args
					(ty-var (raw "a"))))
			(ty-var (raw "a"))
			(where
				(method (mod-of "a") (name "a1")
					(args
						(ty-tuple
							(ty-var (raw "a"))
							(ty-var (raw "a"))))
					(ty (name "Str")))))
		(s-type-decl
			(header (name "C")
				(args
					(ty-var (raw "b"))
					(ty-var (raw "b"))))
			(ty-tuple
				(ty-var (raw "a"))
				(ty-var (raw "b"))))
		(s-type-decl
			(header (name "D")
				(args
					(ty-var (raw "a"))
					(ty-var (raw "b"))))
			(ty-apply
				(ty (name "C"))
				(ty-var (raw "a"))
				(ty-var (raw "b"))))))
~~~
# FORMATTED
~~~roc
A(a) : a where [a.a1 : (a, a) -> Str]

C(b, b) : (a, b)

D(a, b) : C(a, b)
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(s-alias-decl
		(ty-header (name "A")
			(ty-args
				(ty-rigid-var (name "a"))))
		(ty-rigid-var-lookup (ty-rigid-var (name "a"))))
	(s-alias-decl
		(ty-header (name "C")
			(ty-args
				(ty-rigid-var (name "b"))))
		(ty-tuple
			(ty-malformed)
			(ty-rigid-var-lookup (ty-rigid-var (name "b")))))
	(s-alias-decl
		(ty-header (name "D")
			(ty-args
				(ty-rigid-var (name "a"))
				(ty-rigid-var (name "b"))))
		(ty-apply (name "C") (local)
			(ty-rigid-var-lookup (ty-rigid-var (name "a")))
			(ty-rigid-var-lookup (ty-rigid-var (name "b"))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(type_decls
		(alias (type "A(a)")
			(ty-header (name "A")
				(ty-args
					(ty-rigid-var (name "a")))))
		(alias (type "Error")
			(ty-header (name "C")
				(ty-args
					(ty-rigid-var (name "b")))))
		(alias (type "Error")
			(ty-header (name "D")
				(ty-args
					(ty-rigid-var (name "a"))
					(ty-rigid-var (name "b"))))))
	(expressions))
~~~
