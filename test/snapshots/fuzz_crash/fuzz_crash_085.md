# META
~~~ini
description=fuzz regression: canonicalize type annotation arg resolves to non-rigid
type=file
~~~
# SOURCE
~~~roc
C(_,b):()D:C(a,b)E:{b:r}F:e r={(){}}
~~~
# EXPECTED
UNDERSCORE IN TYPE ALIAS - fuzz_crash_085.md:1:3:1:4
UNDECLARED TYPE VARIABLE - fuzz_crash_085.md:1:14:1:15
UNDECLARED TYPE VARIABLE - fuzz_crash_085.md:1:16:1:17
UNDECLARED TYPE VARIABLE - fuzz_crash_085.md:1:23:1:24
UNDECLARED TYPE VARIABLE - fuzz_crash_085.md:1:27:1:28
EMPTY TUPLE NOT ALLOWED - fuzz_crash_085.md:1:32:1:34
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Underscore In Type Alias")
		(region (start 1 3) (end 1 4))
		(headline
			(reflow "Underscores are not allowed in type alias declarations."))
		(document
			(source-region (file "fuzz_crash_085.md") (start 1 3) (end 1 4) (annotation error) (line-text "C(_,b):()D:C(a,b)E:{b:r}F:e r={(){}}"))
			(line-break)
			(reflow "Underscores in type annotations mean \"I don't care about this type\", which doesn't make sense when declaring a type. If you need a placeholder type variable, use a named type variable like `a` instead.")))
	(report
		(severity runtime_error)
		(title "Undeclared Type Variable")
		(region (start 1 14) (end 1 15))
		(headline
			(reflow "The type variable ")
			(annotated code "a")
			(reflow " is not declared in this scope."))
		(document
			(reflow "Type variables must be introduced in a type annotation before they can be used.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_085.md") (start 1 14) (end 1 15) (annotation error) (line-text "C(_,b):()D:C(a,b)E:{b:r}F:e r={(){}}"))))
	(report
		(severity runtime_error)
		(title "Undeclared Type Variable")
		(region (start 1 16) (end 1 17))
		(headline
			(reflow "The type variable ")
			(annotated code "b")
			(reflow " is not declared in this scope."))
		(document
			(reflow "Type variables must be introduced in a type annotation before they can be used.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_085.md") (start 1 16) (end 1 17) (annotation error) (line-text "C(_,b):()D:C(a,b)E:{b:r}F:e r={(){}}"))))
	(report
		(severity runtime_error)
		(title "Undeclared Type Variable")
		(region (start 1 23) (end 1 24))
		(headline
			(reflow "The type variable ")
			(annotated code "r")
			(reflow " is not declared in this scope."))
		(document
			(reflow "Type variables must be introduced in a type annotation before they can be used.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_085.md") (start 1 23) (end 1 24) (annotation error) (line-text "C(_,b):()D:C(a,b)E:{b:r}F:e r={(){}}"))))
	(report
		(severity runtime_error)
		(title "Undeclared Type Variable")
		(region (start 1 27) (end 1 28))
		(headline
			(reflow "The type variable ")
			(annotated code "e")
			(reflow " is not declared in this scope."))
		(document
			(reflow "Type variables must be introduced in a type annotation before they can be used.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_085.md") (start 1 27) (end 1 28) (annotation error) (line-text "C(_,b):()D:C(a,b)E:{b:r}F:e r={(){}}"))))
	(report
		(severity runtime_error)
		(title "Empty Tuple Not Allowed")
		(region (start 1 32) (end 1 34))
		(headline
			(reflow "I am part way through parsing this tuple, but it is empty."))
		(document
			(source-region (file "fuzz_crash_085.md") (start 1 32) (end 1 34) (annotation error) (line-text "C(_,b):()D:C(a,b)E:{b:r}F:e r={(){}}"))
			(line-break)
			(reflow "If you want to represent nothing, try using an empty record: ")
			(annotated code "{}")
			(reflow "."))))
~~~
# TOKENS
~~~zig
UpperIdent,NoSpaceOpenRound,Underscore,Comma,LowerIdent,CloseRound,OpColon,NoSpaceOpenRound,CloseRound,UpperIdent,OpColon,UpperIdent,NoSpaceOpenRound,LowerIdent,Comma,LowerIdent,CloseRound,UpperIdent,OpColon,OpenCurly,LowerIdent,OpColon,LowerIdent,CloseCurly,UpperIdent,OpColon,LowerIdent,LowerIdent,OpAssign,OpenCurly,NoSpaceOpenRound,CloseRound,OpenCurly,CloseCurly,CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-type-decl
			(header (name "C")
				(args
					(_)
					(ty-var (raw "b"))))
			(ty-tuple))
		(s-type-decl
			(header (name "D")
				(args))
			(ty-apply
				(ty (name "C"))
				(ty-var (raw "a"))
				(ty-var (raw "b"))))
		(s-type-decl
			(header (name "E")
				(args))
			(ty-record
				(anno-record-field (name "b")
					(ty-var (raw "r")))))
		(s-type-decl
			(header (name "F")
				(args))
			(ty-var (raw "e")))
		(s-decl
			(p-ident (raw "r"))
			(e-block
				(statements
					(e-tuple)
					(e-record))))))
~~~
# FORMATTED
~~~roc
C(_, b) : ()

D : C(a, b)

E : { b : r }

F : e

r = {
	()
	{}
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "r"))
		(e-block
			(s-expr
				(e-runtime-error (tag "empty_tuple")))
			(e-empty_record)))
	(s-alias-decl
		(ty-header (name "C")
			(ty-args
				(ty-underscore)
				(ty-rigid-var (name "b"))))
		(ty-tuple))
	(s-alias-decl
		(ty-header (name "D"))
		(ty-apply (name "C") (local)
			(ty-malformed)
			(ty-malformed)))
	(s-alias-decl
		(ty-header (name "E"))
		(ty-record
			(field (field "b")
				(ty-malformed))))
	(s-alias-decl
		(ty-header (name "F"))
		(ty-malformed)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "{}")))
	(type_decls
		(alias (type "C(Error, b)")
			(ty-header (name "C")
				(ty-args
					(ty-underscore)
					(ty-rigid-var (name "b")))))
		(alias (type "Error")
			(ty-header (name "D")))
		(alias (type "Error")
			(ty-header (name "E")))
		(alias (type "Error")
			(ty-header (name "F"))))
	(expressions
		(expr (type "{}"))))
~~~
