# META
~~~ini
description=An uninitialized mutable var whose annotation introduces an unbound type variable is rejected, suggesting `_` to infer the type instead
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

main! = |_| {
    var xs : List(a)
    {}
}
~~~
# EXPECTED
UNUSED VARIABLE - var_polymorphic_annotation_uninitialized_rejected.md:4:5:4:21
POLYMORPHIC VAR - var_polymorphic_annotation_uninitialized_rejected.md:4:5:4:21
# PROBLEMS

┌─────────────────┐
│ UNUSED VARIABLE ├─ Variable `xs` is defined here and then never used. ──────┐
└┬────────────────┘                                                           │
 │                                                                            │
 │  var xs : List(a)                                                          │
 │  ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾                                                          │
 └────────────────── var_polymorphic_annotation_uninitialized_rejected.md:4:5 ┘

    If you don't need this variable, prefix it with an underscore like `_xs` to
    suppress this warning.


┌─────────────────┐
│ POLYMORPHIC VAR ├─ This var is declared with a polymorphic type ────────────┐
└┬────────────────┘  annotation, but a mutable variable must have a single    │
 │                   concrete type.                                           │
 │                                                                            │
 │  var xs : List(a)                                                          │
 │  ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾                                                          │
 └────────────────── var_polymorphic_annotation_uninitialized_rejected.md:4:5 ┘

    Give it a concrete type, or replace the type variable with `_` to let the
    type be inferred from how the `var` is used.

# TOKENS
~~~zig
KwApp,OpenSquare,LowerIdent,CloseSquare,OpenCurly,LowerIdent,OpColon,KwPlatform,StringStart,StringPart,StringEnd,CloseCurly,
LowerIdent,OpAssign,OpBar,Underscore,OpBar,OpenCurly,
KwVar,LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
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
						(e-record)))))))
~~~
# FORMATTED
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

main! = |_| {
	var xs : List(a)
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
				(s-var-uninitialized
					(p-assign (ident "xs")))
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
