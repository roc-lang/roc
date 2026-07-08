# META
~~~ini
description=Simple scoping violation test - inner_val should not be visible in outer scope
type=snippet
~~~
# SOURCE
~~~roc
Outer := [A].{
    Inner := [B].{
        inner_val = 100
    }

    outer_val = inner_val
}
~~~
# EXPECTED
NAME NOT IN SCOPE - simple_scope_test.md:6:17:6:26
# PROBLEMS

┌───────────────────┐
│ NAME NOT IN SCOPE ├─ Nothing is named `inner_val` in this scope. ───────────┐
└┬──────────────────┘                                                         │
 │                                                                            │
 │  outer_val = inner_val                                                     │
 │              ‾‾‾‾‾‾‾‾‾                                                     │
 └───────────────────────────────────────────────── simple_scope_test.md:6:17 ┘

    Is it misspelled, or is there an import missing?

# TOKENS
~~~zig
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,Int,
CloseCurly,
LowerIdent,OpAssign,LowerIdent,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-type-decl
			(header (name "Outer")
				(args))
			(ty-tag-union
				(tags
					(ty (name "A"))))
			(associated
				(s-type-decl
					(header (name "Inner")
						(args))
					(ty-tag-union
						(tags
							(ty (name "B"))))
					(associated
						(s-decl
							(p-ident (raw "inner_val"))
							(e-int (raw "100")))))
				(s-decl
					(p-ident (raw "outer_val"))
					(e-ident (raw "inner_val")))))))
~~~
# FORMATTED
~~~roc
Outer := [A].{
	Inner := [B].{
		inner_val = 100
	}

	outer_val = inner_val
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "simple_scope_test.Outer.Inner.inner_val"))
		(e-num (value "100")))
	(d-let
		(p-assign (ident "simple_scope_test.Outer.outer_val"))
		(e-runtime-error (tag "ident_not_in_scope")))
	(s-nominal-decl
		(ty-header (name "Outer"))
		(ty-tag-union
			(ty-tag-name (name "A"))))
	(s-nominal-decl
		(ty-header (name "simple_scope_test.Outer.Inner"))
		(ty-tag-union
			(ty-tag-name (name "B")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Dec"))
		(patt (type "Error")))
	(type_decls
		(nominal (type "Outer")
			(ty-header (name "Outer")))
		(nominal (type "Outer.Inner")
			(ty-header (name "simple_scope_test.Outer.Inner"))))
	(expressions
		(expr (type "Dec"))
		(expr (type "Error"))))
~~~
