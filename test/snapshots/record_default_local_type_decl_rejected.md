# META
~~~ini
description=A `??` default is rejected on a block-local nominal type declaration: defaults are only legal on mod top-level nominal declarations, so a local declaration's default (which could capture function locals) reports and drops
type=snippet
~~~
# SOURCE
~~~roc
f = |n| {
    Cfg := { a : U8 ?? n }
    g = |{}| Cfg.{ a: 1 }
    g({})
}
~~~
# EXPECTED
DEFAULT NOT ALLOWED ON LOCAL TYPE DECLARATION - record_default_local_type_decl_rejected.md:2:14:2:25
UNUSED VARIABLE - record_default_local_type_decl_rejected.md:1:6:1:7
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Default Not Allowed On Local Type Declaration")
		(region (start 2 14) (end 2 25))
		(headline
			(reflow "Field defaults (")
			(annotated code "??")
			(reflow ") are only allowed on nominal type declarations at the top level of a mod, not on type declarations inside a function or block."))
		(document
			(source-region (file "record_default_local_type_decl_rejected.md") (start 2 14) (end 2 25) (annotation error) (line-text "    Cfg := { a : U8 ?? n }"))
			(line-break)
			(line-break)
			(annotated emphasis "Hint:")
			(reflow " A default is materialized at every construction site that omits the field, so it cannot depend on the locals of one function. Move the type declaration to the mod top level, or remove the default.")))
	(report
		(severity warning)
		(title "Unused Variable")
		(region (start 1 6) (end 1 7))
		(headline
			(reflow "Variable ")
			(annotated symbol-unqualified "n")
			(reflow " is defined here and then never used:"))
		(document
			(reflow "If you don't need this variable, prefix it with an underscore like ")
			(annotated symbol-unqualified "_n")
			(reflow " to suppress this warning.")
			(line-break)
			(source-region (file "record_default_local_type_decl_rejected.md") (start 1 6) (end 1 7) (annotation error) (line-text "f = |n| {")))))
~~~
# TOKENS
~~~zig
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenCurly,
UpperIdent,OpColonEqual,OpenCurly,LowerIdent,OpColon,UpperIdent,OpDoubleQuestion,LowerIdent,CloseCurly,
LowerIdent,OpAssign,OpBar,OpenCurly,CloseCurly,OpBar,UpperIdent,Dot,OpenCurly,LowerIdent,OpColon,Int,CloseCurly,
LowerIdent,NoSpaceOpenRound,OpenCurly,CloseCurly,CloseRound,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-decl
			(p-ident (raw "f"))
			(e-lambda
				(args
					(p-ident (raw "n")))
				(e-block
					(statements
						(s-type-decl
							(header (name "Cfg")
								(args))
							(ty-record
								(anno-record-field (name "a")
									(ty (name "U8"))
									(default
										(e-ident (raw "n"))))))
						(s-decl
							(p-ident (raw "g"))
							(e-lambda
								(args
									(p-record))
								(e-nominal-record
									(mapper (e-tag (raw "Cfg")))
									(backing (e-record
											(field (field "a")
												(e-int (raw "1"))))))))
						(e-apply
							(e-ident (raw "g"))
							(e-record))))))))
~~~
# FORMATTED
~~~roc
f = |n| {
	Cfg := { a : U8 ?? n }
	g = |{}| Cfg.{ a: 1 }
	g({})
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "f"))
		(e-lambda
			(args
				(p-assign (ident "n")))
			(e-block
				(s-nominal-decl
					(ty-header (name "Cfg"))
					(ty-record
						(field (field "a")
							(ty-lookup (name "U8") (builtin)))))
				(s-let
					(p-assign (ident "g"))
					(e-lambda
						(args
							(p-record-destructure
								(destructs)))
						(e-nominal (nominal "Cfg")
							(e-record
								(fields
									(field (name "a")
										(e-num (value "1"))))))))
				(e-call (constraint-fn-var 246)
					(e-lookup-local
						(p-assign (ident "g")))
					(e-empty_record)))))
	(s-nominal-decl
		(ty-header (name "Cfg"))
		(ty-record
			(field (field "a")
				(ty-lookup (name "U8") (builtin))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "_arg -> Cfg")))
	(type_decls
		(nominal (type "Cfg")
			(ty-header (name "Cfg"))))
	(expressions
		(expr (type "_arg -> Cfg"))))
~~~
