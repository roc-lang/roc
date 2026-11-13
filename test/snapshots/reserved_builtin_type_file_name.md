# META
~~~ini
description=A type file named after a builtin type is an error, because builtin type names are reserved (issue #12720)
type=file:Bool.roc
~~~
# SOURCE
~~~roc
Bool :: [].{
    level = |s| "user level ${s}"
}
~~~
# EXPECTED
RESERVED MOD NAME - reserved_builtin_type_file_name.md
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Reserved Mod Name")
		(file "reserved_builtin_type_file_name.md")
		(headline
			(text "This file is named ")
			(annotated code "Bool")
			(reflow ".roc, but ")
			(annotated code "Bool")
			(reflow " is the name of a builtin type."))
		(document
			(reflow "Mod names that match builtin types are reserved for those types, so no mod can be named ")
			(annotated code "Bool")
			(reflow ".")
			(line-break)
			(line-break)
			(reflow "Rename this mod to something else.")
			(line-break))))
~~~
# TOKENS
~~~zig
UpperIdent,OpDoubleColon,OpenSquare,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,StringStart,StringPart,OpenStringInterpolation,LowerIdent,CloseStringInterpolation,StringPart,StringEnd,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-type-decl
			(header (name "Bool")
				(args))
			(ty-tag-union
				(tags))
			(associated
				(s-decl
					(p-ident (raw "level"))
					(e-lambda
						(args
							(p-ident (raw "s")))
						(e-string
							(e-string-part (raw "user level "))
							(e-ident (raw "s"))
							(e-string-part (raw "")))))))))
~~~
# FORMATTED
~~~roc
Bool :: [].{
	level = |s| "user level ${s}"
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "Bool.level"))
		(e-lambda
			(args
				(p-assign (ident "s")))
			(e-block
				(s-let
					(p-assign (ident "#interp_0"))
					(e-lookup-local
						(p-assign (ident "s"))))
				(e-interpolation (constraint-fn-var 237) (dispatcher-var 13)
					(first
						(e-literal (string "user level ")))
					(parts
						(e-lookup-local
							(p-assign (ident "#interp_0")))
						(e-literal (string "")))))))
	(s-nominal-decl
		(ty-header (name "Bool"))
		(ty-tag-union)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "item -> a where [a.from_interpolation : List(Str) -> Try(Iter(item) -> a, [InvalidInterpolation(Str)])]")))
	(type_decls
		(nominal (type "Bool")
			(ty-header (name "Bool"))))
	(expressions
		(expr (type "item -> a where [a.from_interpolation : List(Str) -> Try(Iter(item) -> a, [InvalidInterpolation(Str)])]"))))
~~~
