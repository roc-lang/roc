# META
~~~ini
description=Formatter preserves var keyword in record field annotations
type=snippet
~~~
# SOURCE
~~~roc
f=||{var c:[]}
~~~
# EXPECTED
VAR NAME MISSING `$` - fmt_var_in_record_field.md:1:10:1:11
UNUSED VARIABLE - fmt_var_in_record_field.md:1:10:1:11
# PROBLEMS
── ● var name missing `$` ────────────────────── fmt_var_in_record_field.md:1:10

The mutable binding c is declared with var but its name does not start with $.

f=||{var c:[]}
         ^

Rename this binding and all of its uses to $c. The name is only a convention;
mutability comes from the var declaration.

── ● unused variable ─────────────────────────── fmt_var_in_record_field.md:1:10

Variable c is defined here and then never used:

f=||{var c:[]}
         ^

If you don't need this variable, prefix it with an underscore like _c to
suppress this warning.

# TOKENS
~~~zig
LowerIdent,OpAssign,OpBar,OpBar,OpenCurly,KwVar,LowerIdent,OpColon,OpenSquare,CloseSquare,CloseCurly,
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
				(args)
				(e-block
					(statements
						(s-type-anno (name "c")
							(ty-tag-union
								(tags)))))))))
~~~
# FORMATTED
~~~roc
f = || {
	var c : []
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "f"))
		(e-lambda
			(args)
			(e-block
				(s-var-uninitialized
					(p-var-assign (ident "c")))
				(e-empty_record)))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "({}) -> {}")))
	(expressions
		(expr (type "({}) -> {}"))))
~~~
