# META
~~~ini
description=Module scope must not resolve a nested associated item through only the nested type's bare name
type=file:AliasLeak.roc
~~~
# SOURCE
~~~roc
Parent1 := [P1].{
    Nested := [N1].{
        val = 1
    }
}

Parent2 := [P2].{
    Nested := [N2].{
        val = 2
    }
}

bad = Nested.val
good1 = Parent1.Nested.val
good2 = Parent2.Nested.val
~~~
# EXPECTED
DOES NOT EXIST - canon_revamp_nested_short_alias_not_module.md:13:7:13:17
# PROBLEMS

┌────────────────┐
│ DOES NOT EXIST ├─ `Nested.val` does not exist. ─────────────────────────────┐
└┬───────────────┘                                                            │
 │                                                                            │
 │  bad = Nested.val                                                          │
 │        ‾‾‾‾‾‾‾‾‾‾                                                          │
 └──────────────────────── canon_revamp_nested_short_alias_not_module.md:13:7 ┘


# TOKENS
~~~zig
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,Int,
CloseCurly,
CloseCurly,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,Int,
CloseCurly,
CloseCurly,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotLowerIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,NoSpaceDotLowerIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,NoSpaceDotLowerIdent,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-type-decl
			(header (name "Parent1")
				(args))
			(ty-tag-union
				(tags
					(ty (name "P1"))))
			(associated
				(s-type-decl
					(header (name "Nested")
						(args))
					(ty-tag-union
						(tags
							(ty (name "N1"))))
					(associated
						(s-decl
							(p-ident (raw "val"))
							(e-int (raw "1")))))))
		(s-type-decl
			(header (name "Parent2")
				(args))
			(ty-tag-union
				(tags
					(ty (name "P2"))))
			(associated
				(s-type-decl
					(header (name "Nested")
						(args))
					(ty-tag-union
						(tags
							(ty (name "N2"))))
					(associated
						(s-decl
							(p-ident (raw "val"))
							(e-int (raw "2")))))))
		(s-decl
			(p-ident (raw "bad"))
			(e-ident (raw "Nested.val")))
		(s-decl
			(p-ident (raw "good1"))
			(e-ident (raw "Parent1.Nested.val")))
		(s-decl
			(p-ident (raw "good2"))
			(e-ident (raw "Parent2.Nested.val")))))
~~~
# FORMATTED
~~~roc
Parent1 := [P1].{
	Nested := [N1].{
		val = 1
	}
}

Parent2 := [P2].{
	Nested := [N2].{
		val = 2
	}
}

bad = Nested.val

good1 = Parent1.Nested.val

good2 = Parent2.Nested.val
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "AliasLeak.Parent1.Nested.val"))
		(e-num (value "1")))
	(d-let
		(p-assign (ident "AliasLeak.Parent2.Nested.val"))
		(e-num (value "2")))
	(d-let
		(p-assign (ident "bad"))
		(e-runtime-error (tag "qualified_ident_does_not_exist")))
	(d-let
		(p-assign (ident "good1"))
		(e-lookup-local
			(p-assign (ident "AliasLeak.Parent1.Nested.val"))))
	(d-let
		(p-assign (ident "good2"))
		(e-lookup-local
			(p-assign (ident "AliasLeak.Parent2.Nested.val"))))
	(s-nominal-decl
		(ty-header (name "Parent1"))
		(ty-tag-union
			(ty-tag-name (name "P1"))))
	(s-nominal-decl
		(ty-header (name "AliasLeak.Parent1.Nested"))
		(ty-tag-union
			(ty-tag-name (name "N1"))))
	(s-nominal-decl
		(ty-header (name "Parent2"))
		(ty-tag-union
			(ty-tag-name (name "P2"))))
	(s-nominal-decl
		(ty-header (name "AliasLeak.Parent2.Nested"))
		(ty-tag-union
			(ty-tag-name (name "N2")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Dec"))
		(patt (type "Dec"))
		(patt (type "Error"))
		(patt (type "Dec"))
		(patt (type "Dec")))
	(type_decls
		(nominal (type "Parent1")
			(ty-header (name "Parent1")))
		(nominal (type "Parent1.Nested")
			(ty-header (name "AliasLeak.Parent1.Nested")))
		(nominal (type "Parent2")
			(ty-header (name "Parent2")))
		(nominal (type "Parent2.Nested")
			(ty-header (name "AliasLeak.Parent2.Nested"))))
	(expressions
		(expr (type "Dec"))
		(expr (type "Dec"))
		(expr (type "Error"))
		(expr (type "Dec"))
		(expr (type "Dec"))))
~~~
