# META
~~~ini
description=Test error for redeclaring builtin type name
type=file
~~~
# SOURCE
~~~roc
module [Str]

Str : { value: U64 }
~~~
# EXPECTED
MODULE HEADER DEPRECATED - can_error_redeclare_builtin_type.md:1:1:1:13
SHADOWING - can_error_redeclare_builtin_type.md:3:1:3:4
SHADOWING - can_error_redeclare_builtin_type.md:3:1:3:4
EXPOSED BUT NOT DEFINED - can_error_redeclare_builtin_type.md:1:9:1:12
# PROBLEMS
**MODULE HEADER DEPRECATED**
The `module` header is deprecated.

Type modules (headerless files with a top-level type matching the filename) are now the preferred way to define modules.

Remove the `module` header and ensure your file defines a type that matches the filename.
**can_error_redeclare_builtin_type.md:1:1:1:13:**
```roc
module [Str]
```
^^^^^^^^^^^^


**SHADOWING**
The name `Str` is already defined in this scope.

Choose a different name for this identifier.

**can_error_redeclare_builtin_type.md:3:1:3:4:**
```roc
Str : { value: U64 }
```
^^^


**SHADOWING**
The name `Str` is already defined in this scope.

Choose a different name for this identifier.

**can_error_redeclare_builtin_type.md:3:1:3:4:**
```roc
Str : { value: U64 }
```
^^^


**EXPOSED BUT NOT DEFINED**
The module header says that `Str` is exposed, but it is not defined anywhere in this module.

**can_error_redeclare_builtin_type.md:1:9:1:12:**
```roc
module [Str]
```
        ^^^
You can fix this by either defining `Str` in this module, or by removing it from the list of exposed values.

# TOKENS
~~~zig
KwModule,OpenSquare,UpperIdent,CloseSquare,
UpperIdent,OpColon,OpenCurly,LowerIdent,OpColon,UpperIdent,CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(module
		(exposes
			(exposed-upper-ident (text "Str"))))
	(statements
		(s-type-decl
			(header (name "Str")
				(args))
			(ty-record
				(anno-record-field (name "value")
					(ty (name "U64")))))))
~~~
# FORMATTED
~~~roc
module [Str]

Str : { value : U64 }
~~~
# CANONICALIZE
~~~clojure
(can-ir (empty true))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(expressions))
~~~
