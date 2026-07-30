# META
~~~ini
description=A mutable var whose annotation introduces an unbound type variable is rejected (a var must have a concrete type)
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

main! = |_| {
    xs : List(a)
    var xs = []
    xs = [1]
    {}
}
~~~
# EXPECTED
UNUSED VARIABLE - var_polymorphic_annotation_rejected.md:5:5:5:16
POLYMORPHIC VAR - var_polymorphic_annotation_rejected.md:4:5:4:17
# PROBLEMS

┌─────────────────┐
│ UNUSED VARIABLE ├─ Variable `xs` is defined here and then never used. ──────┐
└┬────────────────┘                                                           │
 │                                                                            │
 │  var xs = []                                                               │
 │  ‾‾‾‾‾‾‾‾‾‾‾                                                               │
 └──────────────────────────────── var_polymorphic_annotation_rejected.md:5:5 ┘

    If you don't need this variable, prefix it with an underscore like `_xs` to
    suppress this warning.


┌─────────────────┐
│ POLYMORPHIC VAR ├─ This var is declared with a polymorphic type ────────────┐
└┬────────────────┘  annotation, but a mutable variable must have a single    │
 │                   concrete type.                                           │
 │                                                                            │
 │  xs : List(a)                                                              │
 │  ‾‾‾‾‾‾‾‾‾‾‾‾                                                              │
 └──────────────────────────────── var_polymorphic_annotation_rejected.md:4:5 ┘

    Give it a concrete type, or replace the type variable with `_` to let the
    type be inferred from how the `var` is used.

# TOKENS
~~~zig
KwApp,OpenSquare,LowerIdent,CloseSquare,OpenCurly,LowerIdent,OpColon,KwPlatform,StringStart,StringPart,StringEnd,CloseCurly,
LowerIdent,OpAssign,OpBar,Underscore,OpBar,OpenCurly,
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
KwVar,LowerIdent,OpAssign,OpenSquare,CloseSquare,
LowerIdent,OpAssign,OpenSquare,Int,CloseSquare,
OpenCurly,CloseCurly,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(app
		(provides
			(exposed-lower-ident
				(text "main!")))
		(record-field (name "pf")
			(e-string
				(e-string-part (raw "../basic-cli/main.roc"))))
		(packages
			(record-field (name "pf")
				(e-string
					(e-string-part (raw "../basic-cli/main.roc"))))))
	(statements
		(s-decl
			(p-ident (raw "main!"))
			(e-lambda
				(args
					(p-underscore))
				(e-block
					(statements
						(s-type-anno (name "xs")
							(ty-apply
								(ty (name "List"))
								(ty-var (raw "a"))))
						(s-var (name "xs")
							(e-list))
						(s-decl
							(p-ident (raw "xs"))
							(e-list
								(e-int (raw "1"))))
						(e-record)))))))
~~~
# FORMATTED
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

main! = |_| {
	xs : List(a)
	var xs = []
	xs = [1]
	{}
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "main!"))
		(e-lambda
			(args
				(p-underscore))
			(e-block
				(s-var
					(p-assign (ident "xs"))
					(e-empty_list))
				(s-reassign
					(p-assign (ident "xs"))
					(e-list
						(elems
							(e-num (value "1")))))
				(e-empty_record)))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "_arg -> {}")))
	(expressions
		(expr (type "_arg -> {}"))))
~~~
