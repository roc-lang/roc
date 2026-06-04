# META
~~~ini
description=Type aliases should not allow underscore-prefixed type parameters like _a
type=snippet
~~~
# SOURCE
~~~roc
# Type alias with underscore-prefixed parameter - should be rejected
AliasType(_a) : _a

# Type alias with multiple parameters including underscore-prefixed
AliasType2(_a, b) : b

# Type alias where underscore comes second
AliasType3(a, _b) : a
~~~
# EXPECTED
UNDERSCORE IN TYPE ALIAS - underscore_prefixed_param_in_type_alias.md:2:11:2:13
UNDERSCORE IN TYPE ALIAS - underscore_prefixed_param_in_type_alias.md:2:17:2:19
UNDERSCORE IN TYPE ALIAS - underscore_prefixed_param_in_type_alias.md:5:12:5:14
UNDERSCORE IN TYPE ALIAS - underscore_prefixed_param_in_type_alias.md:8:15:8:17
# PROBLEMS
**UNDERSCORE IN TYPE ALIAS**
Underscores are not allowed in type alias declarations.

**underscore_prefixed_param_in_type_alias.md:2:11:2:13:**
```roc
AliasType(_a) : _a
```
          ^^

Underscores in type annotations mean "I don't care about this type", which doesn't make sense when declaring a type. If you need a placeholder type variable, use a named type variable like `a` instead.

**UNDERSCORE IN TYPE ALIAS**
Underscores are not allowed in type alias declarations.

**underscore_prefixed_param_in_type_alias.md:2:17:2:19:**
```roc
AliasType(_a) : _a
```
                ^^

Underscores in type annotations mean "I don't care about this type", which doesn't make sense when declaring a type. If you need a placeholder type variable, use a named type variable like `a` instead.

**UNDERSCORE IN TYPE ALIAS**
Underscores are not allowed in type alias declarations.

**underscore_prefixed_param_in_type_alias.md:5:12:5:14:**
```roc
AliasType2(_a, b) : b
```
           ^^

Underscores in type annotations mean "I don't care about this type", which doesn't make sense when declaring a type. If you need a placeholder type variable, use a named type variable like `a` instead.

**UNDERSCORE IN TYPE ALIAS**
Underscores are not allowed in type alias declarations.

**underscore_prefixed_param_in_type_alias.md:8:15:8:17:**
```roc
AliasType3(a, _b) : a
```
              ^^

Underscores in type annotations mean "I don't care about this type", which doesn't make sense when declaring a type. If you need a placeholder type variable, use a named type variable like `a` instead.

# TOKENS
~~~zig
UpperIdent,NoSpaceOpenRound,NamedUnderscore,CloseRound,OpColon,NamedUnderscore,
UpperIdent,NoSpaceOpenRound,NamedUnderscore,Comma,LowerIdent,CloseRound,OpColon,LowerIdent,
UpperIdent,NoSpaceOpenRound,LowerIdent,Comma,NamedUnderscore,CloseRound,OpColon,LowerIdent,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-type-decl
			(header (name "AliasType")
				(args
					(underscore-ty-var (raw "_a"))))
			(underscore-ty-var (raw "_a")))
		(s-type-decl
			(header (name "AliasType2")
				(args
					(underscore-ty-var (raw "_a"))
					(ty-var (raw "b"))))
			(ty-var (raw "b")))
		(s-type-decl
			(header (name "AliasType3")
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
	(s-alias-decl
		(ty-header (name "AliasType")
			(ty-args
				(ty-rigid-var (name "_a"))))
		(ty-rigid-var-lookup (ty-rigid-var (name "_a"))))
	(s-alias-decl
		(ty-header (name "AliasType2")
			(ty-args
				(ty-rigid-var (name "_a"))
				(ty-rigid-var (name "b"))))
		(ty-rigid-var-lookup (ty-rigid-var (name "b"))))
	(s-alias-decl
		(ty-header (name "AliasType3")
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
		(alias (type "AliasType(_a)")
			(ty-header (name "AliasType")
				(ty-args
					(ty-rigid-var (name "_a")))))
		(alias (type "AliasType2(_a, b)")
			(ty-header (name "AliasType2")
				(ty-args
					(ty-rigid-var (name "_a"))
					(ty-rigid-var (name "b")))))
		(alias (type "AliasType3(a, _b)")
			(ty-header (name "AliasType3")
				(ty-args
					(ty-rigid-var (name "a"))
					(ty-rigid-var (name "_b"))))))
	(expressions))
~~~
