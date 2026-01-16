# META
~~~ini
description=Variable scoping with var keyword
type=snippet
~~~
# SOURCE
~~~roc
# Test var redeclaration (should produce shadowing warning)
redeclareTest = |_| {
	var $x = 5
	var $x = 10 # Redeclare var - should warn but proceed
	$x = 15 # Reassign - should work without warning
	$x
}

result = redeclareTest({})
~~~
# EXPECTED
DUPLICATE DEFINITION - can_var_scoping_var_redeclaration.md:4:2:4:13
# PROBLEMS
**DUPLICATE DEFINITION**
The name `$x` is being redeclared in this scope.

The redeclaration is here:
**can_var_scoping_var_redeclaration.md:4:2:4:13:**
```roc
	var $x = 10 # Redeclare var - should warn but proceed
```
	^^^^^^^^^^^

But `$x` was already defined here:
**can_var_scoping_var_redeclaration.md:3:2:3:12:**
```roc
	var $x = 5
```
	^^^^^^^^^^


# TOKENS
~~~zig
LowerIdent,OpAssign,OpBar,Underscore,OpBar,OpenCurly,
KwVar,LowerIdent,OpAssign,Int,
KwVar,LowerIdent,OpAssign,Int,
LowerIdent,OpAssign,Int,
LowerIdent,
CloseCurly,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,OpenCurly,CloseCurly,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-decl
			(p-ident (raw "redeclareTest"))
			(e-lambda
				(args
					(p-underscore))
				(e-block
					(statements
						(s-var (name "$x")
							(e-int (raw "5")))
						(s-var (name "$x")
							(e-int (raw "10")))
						(s-decl
							(p-ident (raw "$x"))
							(e-int (raw "15")))
						(e-ident (raw "$x"))))))
		(s-decl
			(p-ident (raw "result"))
			(e-apply
				(e-ident (raw "redeclareTest"))
				(e-record)))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "redeclareTest"))
		(e-lambda
			(args
				(p-underscore))
			(e-block
				(s-var
					(p-assign (ident "$x"))
					(e-num (value "5")))
				(s-var
					(p-assign (ident "$x"))
					(e-num (value "10")))
				(s-reassign
					(p-assign (ident "$x"))
					(e-num (value "15")))
				(e-lookup-local
					(p-assign (ident "$x"))))))
	(d-let
		(p-assign (ident "result"))
		(e-call
			(e-lookup-local
				(p-assign (ident "redeclareTest")))
			(e-empty_record))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "_arg -> a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))
		(patt (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]")))
	(expressions
		(expr (type "_arg -> a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))
		(expr (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))))
~~~
