# META
~~~ini
description=Var declaration without $ prefix should warn
type=snippet
~~~
# SOURCE
~~~roc
result = {
	var count = 0
	count = count + 1
	count
}
~~~
# EXPECTED
VAR WITHOUT $ PREFIX - var_without_dollar_prefix.md:2:2:2:15
# PROBLEMS
**VAR WITHOUT $ PREFIX**
This `var` is named `count` but variables declared with `var` should start with `$` to indicate they are mutable.

Suggestion: rename `count` to `$count`.

**var_without_dollar_prefix.md:2:2:2:15:**
```roc
	var count = 0
```
	^^^^^^^^^^^^^


# TOKENS
~~~zig
LowerIdent,OpAssign,OpenCurly,
KwVar,LowerIdent,OpAssign,Int,
LowerIdent,OpAssign,LowerIdent,OpPlus,Int,
LowerIdent,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-decl
			(p-ident (raw "result"))
			(e-block
				(statements
					(s-var (name "count")
						(e-int (raw "0")))
					(s-decl
						(p-ident (raw "count"))
						(e-binop (op "+")
							(e-ident (raw "count"))
							(e-int (raw "1"))))
					(e-ident (raw "count")))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "result"))
		(e-block
			(s-var
				(p-assign (ident "count"))
				(e-num (value "0")))
			(s-reassign
				(p-assign (ident "count"))
				(e-binop (op "add")
					(e-lookup-local
						(p-assign (ident "count")))
					(e-num (value "1"))))
			(e-lookup-local
				(p-assign (ident "count"))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)]), a.plus : a, b -> a, b.from_numeral : Numeral -> Try(b, [InvalidNumeral(Str)])]")))
	(expressions
		(expr (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)]), a.plus : a, b -> a, b.from_numeral : Numeral -> Try(b, [InvalidNumeral(Str)])]"))))
~~~
