# META
~~~ini
description=Nominal types should allow underscore-prefixed type parameters like _a
type=snippet
~~~
# SOURCE
~~~roc
# Nominal type with underscore-prefixed parameter - should be allowed
NominalType(_a) := Str

# Nominal type with multiple parameters including underscore-prefixed
NominalType2(_a, b) := b

# Nominal type where underscore comes second
NominalType3(a, _b) := a
~~~
# EXPECTED
TYPE REDECLARED - underscore_prefixed_param_in_nominal_type.md:5:1:5:25
TYPE REDECLARED - underscore_prefixed_param_in_nominal_type.md:8:1:8:25
# PROBLEMS
**TYPE REDECLARED**
The type _NominalType2_ is being redeclared.

The redeclaration is here:
**underscore_prefixed_param_in_nominal_type.md:5:1:5:25:**
```roc
NominalType2(_a, b) := b
```
^^^^^^^^^^^^^^^^^^^^^^^^

But _NominalType2_ was already declared here:
**underscore_prefixed_param_in_nominal_type.md:5:1:5:25:**
```roc
NominalType2(_a, b) := b
```
^^^^^^^^^^^^^^^^^^^^^^^^


**TYPE REDECLARED**
The type _NominalType3_ is being redeclared.

The redeclaration is here:
**underscore_prefixed_param_in_nominal_type.md:8:1:8:25:**
```roc
NominalType3(a, _b) := a
```
^^^^^^^^^^^^^^^^^^^^^^^^

But _NominalType3_ was already declared here:
**underscore_prefixed_param_in_nominal_type.md:8:1:8:25:**
```roc
NominalType3(a, _b) := a
```
^^^^^^^^^^^^^^^^^^^^^^^^


# TOKENS
~~~zig
UpperIdent,NoSpaceOpenRound,NamedUnderscore,CloseRound,OpColonEqual,UpperIdent,
UpperIdent,NoSpaceOpenRound,NamedUnderscore,Comma,LowerIdent,CloseRound,OpColonEqual,LowerIdent,
UpperIdent,NoSpaceOpenRound,LowerIdent,Comma,NamedUnderscore,CloseRound,OpColonEqual,LowerIdent,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-type-decl
			(header (name "NominalType")
				(args
					(underscore-ty-var (raw "_a"))))
			(ty (name "Str")))
		(s-type-decl
			(header (name "NominalType2")
				(args
					(underscore-ty-var (raw "_a"))
					(ty-var (raw "b"))))
			(ty-var (raw "b")))
		(s-type-decl
			(header (name "NominalType3")
				(args
					(ty-var (raw "a"))
					(underscore-ty-var (raw "_b"))))
			(ty-var (raw "a")))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(s-nominal-decl
		(ty-header (name "NominalType")
			(ty-args
				(ty-rigid-var (name "_a"))))
		(ty-lookup (name "Str") (builtin)))
	(s-nominal-decl
		(ty-header (name "NominalType2")
			(ty-args
				(ty-rigid-var (name "_a"))
				(ty-rigid-var (name "b"))))
		(ty-rigid-var-lookup (ty-rigid-var (name "b"))))
	(s-nominal-decl
		(ty-header (name "NominalType3")
			(ty-args
				(ty-rigid-var (name "a"))
				(ty-rigid-var (name "_b"))))
		(ty-rigid-var-lookup (ty-rigid-var (name "a")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(type_decls
		(nominal (type "NominalType(_a)")
			(ty-header (name "NominalType")
				(ty-args
					(ty-rigid-var (name "_a")))))
		(nominal (type "NominalType2(_a, b)")
			(ty-header (name "NominalType2")
				(ty-args
					(ty-rigid-var (name "_a"))
					(ty-rigid-var (name "b")))))
		(nominal (type "NominalType3(a, _b)")
			(ty-header (name "NominalType3")
				(ty-args
					(ty-rigid-var (name "a"))
					(ty-rigid-var (name "_b"))))))
	(expressions))
~~~
