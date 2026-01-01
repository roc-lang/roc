# META
~~~ini
description=Return statement outside function
type=file
~~~
# SOURCE
~~~roc
module [main]

return 42
~~~
# EXPECTED
MODULE HEADER DEPRECATED - parse_error_return_outside_fn.md:1:1:1:14
INVALID STATEMENT - parse_error_return_outside_fn.md:3:1:3:10
EXPOSED BUT NOT DEFINED - parse_error_return_outside_fn.md:1:9:1:13
# PROBLEMS
**MODULE HEADER DEPRECATED**
The `module` header is deprecated.

Type modules (headerless files with a top-level type matching the filename) are now the preferred way to define modules.

Remove the `module` header and ensure your file defines a type that matches the filename.
**parse_error_return_outside_fn.md:1:1:1:14:**
```roc
module [main]
```
^^^^^^^^^^^^^


**INVALID STATEMENT**
The statement `return` is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**parse_error_return_outside_fn.md:3:1:3:10:**
```roc
return 42
```
^^^^^^^^^


**EXPOSED BUT NOT DEFINED**
The module header says that `main` is exposed, but it is not defined anywhere in this module.

**parse_error_return_outside_fn.md:1:9:1:13:**
```roc
module [main]
```
        ^^^^
You can fix this by either defining `main` in this module, or by removing it from the list of exposed values.

# TOKENS
~~~zig
KwModule,OpenSquare,LowerIdent,CloseSquare,
KwReturn,Int,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(module
		(exposes
			(exposed-lower-ident
				(text "main"))))
	(statements
		(s-return
			(e-int (raw "42")))))
~~~
# FORMATTED
~~~roc
NO CHANGE
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
