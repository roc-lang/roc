# META
~~~ini
description=A module that exposes a binding aliasing a polymorphic function
type=file
~~~
# SOURCE
~~~roc
module [shorthand]

FooBar := {}.{
    myfunc : List(a) -> U64
    myfunc = |list| list.len()
}

shorthand = FooBar.myfunc
~~~
# EXPECTED
MODULE HEADER DEPRECATED - generalize_alias_exposed.md:1:1:1:19
# PROBLEMS
                                                    ┌──────────────────────────┐
┌─ The module header is deprecated. ────────────────┤ MODULE HEADER DEPRECATED │
│                                                   └─────────────────────────┬┘
│                                                                             │
│  module [shorthand]                                                         │
│  ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾                                                         │
└─────────────────────────────────────────────────────────────────────────────┘
    generalize_alias_exposed.md:1:1

    Type modules (headerless files with a top-level type matching the filename)
    are now the preferred way to define modules.

    Remove the module header and ensure your file defines a type that matches
    the filename.
# TOKENS
~~~zig
KwModule,OpenSquare,LowerIdent,CloseSquare,
UpperIdent,OpColonEqual,OpenCurly,CloseCurly,Dot,OpenCurly,
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,LowerIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,CloseRound,
CloseCurly,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotLowerIdent,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(module
		(exposes
			(exposed-lower-ident
				(text "shorthand"))))
	(statements
		(s-type-decl
			(header (name "FooBar")
				(args))
			(ty-record)
			(associated
				(s-type-anno (name "myfunc")
					(ty-fn
						(ty-apply
							(ty (name "List"))
							(ty-var (raw "a")))
						(ty (name "U64"))))
				(s-decl
					(p-ident (raw "myfunc"))
					(e-lambda
						(args
							(p-ident (raw "list")))
						(e-method-call (method ".len")
							(receiver
								(e-ident (raw "list")))
							(args))))))
		(s-decl
			(p-ident (raw "shorthand"))
			(e-ident (raw "FooBar.myfunc")))))
~~~
# FORMATTED
~~~roc
module [shorthand]

FooBar := {}.{
	myfunc : List(a) -> U64
	myfunc = |list| list.len()
}

shorthand = FooBar.myfunc
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "generalize_alias_exposed.FooBar.myfunc"))
		(e-lambda
			(args
				(p-assign (ident "list")))
			(e-dispatch-call (method "len") (constraint-fn-var 39)
				(receiver
					(e-lookup-local
						(p-assign (ident "list"))))
				(args)))
		(annotation
			(ty-fn (effectful false)
				(ty-apply (name "List") (builtin)
					(ty-rigid-var (name "a")))
				(ty-lookup (name "U64") (builtin)))))
	(d-let
		(p-assign (ident "shorthand"))
		(e-lookup-local
			(p-assign (ident "generalize_alias_exposed.FooBar.myfunc"))))
	(s-nominal-decl
		(ty-header (name "FooBar"))
		(ty-record)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "List(a) -> U64"))
		(patt (type "List(a) -> U64")))
	(type_decls
		(nominal (type "FooBar")
			(ty-header (name "FooBar"))))
	(expressions
		(expr (type "List(a) -> U64"))
		(expr (type "List(a) -> U64"))))
~~~
