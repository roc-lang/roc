# META
~~~ini
description=Parameterized nominal/alias types referenced without type arguments report an arity mismatch instead of crashing a later stage
type=file:Wrapper.roc
~~~
# SOURCE
~~~roc
Wrapper(a) := [Wrap(a)]

Pair(a) : (a, a)

use_nominal : Wrapper -> Wrapper
use_nominal = |w| w

use_alias : Pair -> Pair
use_alias = |p| p
~~~
# EXPECTED
TOO FEW ARGS - type_apply_bare_parameterized_too_few_args.md:5:15:5:22
TOO FEW ARGS - type_apply_bare_parameterized_too_few_args.md:5:26:5:33
TOO FEW ARGS - type_apply_bare_parameterized_too_few_args.md:8:13:8:17
TOO FEW ARGS - type_apply_bare_parameterized_too_few_args.md:8:21:8:25
# PROBLEMS
**TOO FEW ARGS**
The type _Wrapper_ expects 1 argument, but got 0 instead.
**type_apply_bare_parameterized_too_few_args.md:5:15:5:22:**
```roc
use_nominal : Wrapper -> Wrapper
```
              ^^^^^^^


**TOO FEW ARGS**
The type _Wrapper_ expects 1 argument, but got 0 instead.
**type_apply_bare_parameterized_too_few_args.md:5:26:5:33:**
```roc
use_nominal : Wrapper -> Wrapper
```
                         ^^^^^^^


**TOO FEW ARGS**
The type _Pair_ expects 1 argument, but got 0 instead.
**type_apply_bare_parameterized_too_few_args.md:8:13:8:17:**
```roc
use_alias : Pair -> Pair
```
            ^^^^


**TOO FEW ARGS**
The type _Pair_ expects 1 argument, but got 0 instead.
**type_apply_bare_parameterized_too_few_args.md:8:21:8:25:**
```roc
use_alias : Pair -> Pair
```
                    ^^^^


# TOKENS
~~~zig
UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpColonEqual,OpenSquare,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,CloseSquare,
UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpColon,OpenRound,LowerIdent,Comma,LowerIdent,CloseRound,
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,LowerIdent,
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,LowerIdent,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-type-decl
			(header (name "Wrapper")
				(args
					(ty-var (raw "a"))))
			(ty-tag-union
				(tags
					(ty-apply
						(ty (name "Wrap"))
						(ty-var (raw "a"))))))
		(s-type-decl
			(header (name "Pair")
				(args
					(ty-var (raw "a"))))
			(ty-tuple
				(ty-var (raw "a"))
				(ty-var (raw "a"))))
		(s-type-anno (name "use_nominal")
			(ty-fn
				(ty (name "Wrapper"))
				(ty (name "Wrapper"))))
		(s-decl
			(p-ident (raw "use_nominal"))
			(e-lambda
				(args
					(p-ident (raw "w")))
				(e-ident (raw "w"))))
		(s-type-anno (name "use_alias")
			(ty-fn
				(ty (name "Pair"))
				(ty (name "Pair"))))
		(s-decl
			(p-ident (raw "use_alias"))
			(e-lambda
				(args
					(p-ident (raw "p")))
				(e-ident (raw "p"))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "use_nominal"))
		(e-lambda
			(args
				(p-assign (ident "w")))
			(e-runtime-error (tag "erroneous_value_use")))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "Wrapper") (local))
				(ty-lookup (name "Wrapper") (local)))))
	(d-let
		(p-assign (ident "use_alias"))
		(e-lambda
			(args
				(p-assign (ident "p")))
			(e-runtime-error (tag "erroneous_value_use")))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "Pair") (local))
				(ty-lookup (name "Pair") (local)))))
	(s-nominal-decl
		(ty-header (name "Wrapper")
			(ty-args
				(ty-rigid-var (name "a"))))
		(ty-tag-union
			(ty-tag-name (name "Wrap")
				(ty-rigid-var-lookup (ty-rigid-var (name "a"))))))
	(s-alias-decl
		(ty-header (name "Pair")
			(ty-args
				(ty-rigid-var (name "a"))))
		(ty-tuple
			(ty-rigid-var-lookup (ty-rigid-var (name "a")))
			(ty-rigid-var-lookup (ty-rigid-var (name "a"))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Error -> Error"))
		(patt (type "Error -> Error")))
	(type_decls
		(nominal (type "Wrapper(a)")
			(ty-header (name "Wrapper")
				(ty-args
					(ty-rigid-var (name "a")))))
		(alias (type "Pair(a)")
			(ty-header (name "Pair")
				(ty-args
					(ty-rigid-var (name "a"))))))
	(expressions
		(expr (type "Error -> Error"))
		(expr (type "Error -> Error"))))
~~~
