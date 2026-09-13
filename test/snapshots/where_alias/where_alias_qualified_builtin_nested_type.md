# META
~~~ini
description=A qualified where clause reference to a nested builtin type resolves and is rejected as not a where alias
type=snippet
~~~
# SOURCE
~~~roc
describe : a -> Str where [a.Str.Utf8Problem]
describe = |value| value.to_str()
~~~
# EXPECTED
NOT A WHERE ALIAS - where_alias_qualified_builtin_nested_type.md:1:29:1:45
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Not a Where Alias")
		(region (start 1 29) (end 1 45))
		(headline
			(reflow "A where clause can only name a where alias, but")
			(reflow " ")
			(annotated type "Str.Utf8Problem")
			(reflow " ")
			(reflow "is a type."))
		(document
			(source-region (file "where_alias_qualified_builtin_nested_type.md") (start 1 29) (end 1 45) (annotation error) (line-text "describe : a -> Str where [a.Str.Utf8Problem]"))
			(line-break)
			(reflow "A where alias names a set of method constraints, declared like")
			(reflow " ")
			(annotated code "a.Sortable : where [a.order_relative_to : a -> [Before, Same, After]]")
			(reflow " ")
			(reflow "and written in a where clause as")
			(reflow " ")
			(annotated code "where [a.Sortable]"))))
~~~
# TOKENS
~~~zig
LowerIdent,OpColon,LowerIdent,OpArrow,UpperIdent,KwWhere,OpenSquare,LowerIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,CloseSquare,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,LowerIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-type-anno (name "describe")
			(ty-fn
				(ty-var (raw "a"))
				(ty (name "Str")))
			(where
				(alias (mod-of "a")
					(ty (name "Str.Utf8Problem")))))
		(s-decl
			(p-ident (raw "describe"))
			(e-lambda
				(args
					(p-ident (raw "value")))
				(e-method-call (method ".to_str")
					(receiver
						(e-ident (raw "value")))
					(args))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "describe"))
		(e-runtime-error (tag "erroneous_value_expr"))
		(annotation
			(ty-fn (effectful false)
				(ty-rigid-var (name "a"))
				(ty-lookup (name "Str") (builtin)))
			(where
				(alias
					(ty-rigid-var-lookup (ty-rigid-var (name "a")))
					(ty-lookup (name "Str.Utf8Problem") (builtin)))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Error -> Str")))
	(expressions
		(expr (type "Error -> Str"))))
~~~
