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
UNUSED VARIABLE - fmt_var_in_record_field.md:1:6:1:14
# PROBLEMS
**UNUSED VARIABLE**
Variable `c` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_c` to suppress this warning.
The unused variable is declared here:
**fmt_var_in_record_field.md:1:6:1:14:**
```roc
f=||{var c:[]}
```
     ^^^^^^^^


# TOKENS
~~~zig
LowerIdent,OpAssign,OpBar,OpBar,OpenCurly,KwVar,LowerIdent,OpColon,OpenSquare,CloseSquare,CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
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
		(p-assign (ident "c"))
		(e-anno-only)
		(annotation
			(ty-tag-union)))
	(d-let
		(p-assign (ident "f"))
		(e-lambda
			(args)
			(e-block
				(s-let
					(p-assign (ident "c"))
					(e-anno-only))
				(e-empty_record)))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "[Error]"))
		(patt (type "({}) -> {}")))
	(expressions
		(expr (type "[Error]"))
		(expr (type "({}) -> {}"))))
~~~
