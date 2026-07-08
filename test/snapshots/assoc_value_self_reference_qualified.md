# META
~~~ini
description=An associated value whose RHS references the value being defined through its qualified name reports INVALID ASSIGNMENT TO ITSELF instead of manufacturing a self-cycle
type=file:QualSelf.roc
~~~
# SOURCE
~~~roc
QualSelf := [].{
    with_uri = QualSelf.with_uri
}
~~~
# EXPECTED
INVALID ASSIGNMENT TO ITSELF - assoc_value_self_reference_qualified.md:2:16:2:33
# PROBLEMS

┌──────────────────────────────┐
│ INVALID ASSIGNMENT TO ITSELF ├─ The value `QualSelf.with_uri` is assigned ──┐
└┬─────────────────────────────┘  to itself, which would cause an infinite    │
 │                                loop at runtime.                            │
 │                                                                            │
 │  with_uri = QualSelf.with_uri                                              │
 │             ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾                                              │
 └────────────────────────────── assoc_value_self_reference_qualified.md:2:16 ┘

    Only functions can reference themselves (for recursion). For non-function
    values, the right-hand side must be fully computable without referring to
    the value being assigned.

# TOKENS
~~~zig
UpperIdent,OpColonEqual,OpenSquare,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotLowerIdent,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-type-decl
			(header (name "QualSelf")
				(args))
			(ty-tag-union
				(tags))
			(associated
				(s-decl
					(p-ident (raw "with_uri"))
					(e-ident (raw "QualSelf.with_uri")))))))
~~~
# FORMATTED
~~~roc
QualSelf := [].{
	with_uri = QualSelf.with_uri
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "QualSelf.with_uri"))
		(e-runtime-error (tag "self_referential_definition")))
	(s-nominal-decl
		(ty-header (name "QualSelf"))
		(ty-tag-union)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Error")))
	(type_decls
		(nominal (type "QualSelf")
			(ty-header (name "QualSelf"))))
	(expressions
		(expr (type "Error"))))
~~~
