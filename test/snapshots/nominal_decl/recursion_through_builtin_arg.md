# META
~~~ini
description=Nominal recursion through a builtin type argument (no tag/record indirection) is rejected at the declaration
type=snippet
~~~
# SOURCE
~~~roc
MyList := List(MyList)

t : MyList
t = MyList.([])
~~~
# EXPECTED
INVALID RECURSIVE TYPE - recursion_through_builtin_arg.md:1:1:1:23
# PROBLEMS

┌────────────────────────┐
│ INVALID RECURSIVE TYPE ├─ The nominal type MyList refers to itself in a ────┐
└┬───────────────────────┘  way that would make it infinite.                  │
 │                                                                            │
 │  MyList := List(MyList)                                                    │
 │  ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾                                                    │
 └────────────────────────────────────── recursion_through_builtin_arg.md:1:1 ┘

    Its definition is:

        List(MyList)

    Hint: Recursion in a nominal type is only allowed inside a tag union
    payload or record field — for example `ConsList(a) := [Nil, Cons(a,
    ConsList(a))]`.

# TOKENS
~~~zig
UpperIdent,OpColonEqual,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,UpperIdent,Dot,NoSpaceOpenRound,OpenSquare,CloseSquare,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-type-decl
			(header (name "MyList")
				(args))
			(ty-apply
				(ty (name "List"))
				(ty (name "MyList"))))
		(s-type-anno (name "t")
			(ty (name "MyList")))
		(s-decl
			(p-ident (raw "t"))
			(e-nominal-apply
				(mapper (e-tag (raw "MyList")))
				(e-list)))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "t"))
		(e-nominal (nominal "MyList")
			(e-empty_list))
		(annotation
			(ty-lookup (name "MyList") (local))))
	(s-nominal-decl
		(ty-header (name "MyList"))
		(ty-apply (name "List") (builtin)
			(ty-lookup (name "MyList") (local)))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Error")))
	(type_decls
		(nominal (type "Error")
			(ty-header (name "MyList"))))
	(expressions
		(expr (type "Error"))))
~~~
