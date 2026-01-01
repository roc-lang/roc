# META
~~~ini
description=Var statement without identifier
type=expr
~~~
# SOURCE
~~~roc
|x| { var = 1 }
~~~
# EXPECTED
PARSE ERROR - parse_error_var_no_ident.md:1:11:1:12
UNUSED VARIABLE - parse_error_var_no_ident.md:1:2:1:3
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: `var_must_have_ident`
This is an unexpected parsing error. Please check your syntax.

**parse_error_var_no_ident.md:1:11:1:12:**
```roc
|x| { var = 1 }
```
          ^


**UNUSED VARIABLE**
Variable `x` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_x` to suppress this warning.
The unused variable is declared here:
**parse_error_var_no_ident.md:1:2:1:3:**
```roc
|x| { var = 1 }
```
 ^


# TOKENS
~~~zig
OpBar,LowerIdent,OpBar,OpenCurly,KwVar,OpAssign,Int,CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-lambda
	(args
		(p-ident (raw "x")))
	(e-block
		(statements
			(s-malformed (tag "var_must_have_ident"))
			(e-int (raw "1")))))
~~~
# FORMATTED
~~~roc
|x| {
		1
}
~~~
# CANONICALIZE
~~~clojure
(e-lambda
	(args
		(p-assign (ident "x")))
	(e-block
		(e-num (value "1"))))
~~~
# TYPES
~~~clojure
(expr (type "_arg -> a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))
~~~
