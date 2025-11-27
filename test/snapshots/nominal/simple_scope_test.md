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
UNDEFINED VARIABLE - simple_scope_test.md:6:17:6:26
UNUSED VARIABLE - simple_scope_test.md:6:17:6:26
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `inner_val` in this scope.
Is there an `import` or `exposing` missing up-top?

**simple_scope_test.md:6:17:6:26:**
```roc
    outer_val = inner_val
```
                ^^^^^^^^^


**UNUSED VARIABLE**
Variable `inner_val` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_inner_val` to suppress this warning.
The unused variable is declared here:
**simple_scope_test.md:6:17:6:26:**
```roc
    outer_val = inner_val
```
                ^^^^^^^^^


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
		(e-lookup-local
			(p-assign (ident "inner_val"))))
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
		(patt (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))
		(patt (type "_a")))
	(type_decls
		(nominal (type "Outer")
			(ty-header (name "Outer")))
		(nominal (type "Outer.Inner")
			(ty-header (name "simple_scope_test.Outer.Inner"))))
	(expressions
		(expr (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))
		(expr (type "_a"))))
~~~
