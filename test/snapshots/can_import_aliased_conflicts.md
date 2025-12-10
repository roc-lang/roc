# META
~~~ini
description=Import alias name conflicts
type=snippet
~~~
# SOURCE
~~~roc
import json.Json as MyModule
import http.Client as MyModule

main = {
    x = MyModule.parse
    x
}
~~~
# EXPECTED
DUPLICATE DEFINITION - can_import_aliased_conflicts.md:2:1:2:31
UNDEFINED VARIABLE - can_import_aliased_conflicts.md:5:9:5:23
# PROBLEMS
**DUPLICATE DEFINITION**
The name `MyModule` is being redeclared in this scope.

The redeclaration is here:
**can_import_aliased_conflicts.md:2:1:2:31:**
```roc
import http.Client as MyModule
```
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

But `MyModule` was already defined here:
**can_import_aliased_conflicts.md:1:1:1:1:**
```roc
import json.Json as MyModule
```
^


**UNDEFINED VARIABLE**
Nothing is named `parse` in this scope.
Is there an `import` or `exposing` missing up-top?

**can_import_aliased_conflicts.md:5:9:5:23:**
```roc
    x = MyModule.parse
```
        ^^^^^^^^^^^^^^


# TOKENS
~~~zig
KwImport,LowerIdent,NoSpaceDotUpperIdent,KwAs,UpperIdent,
KwImport,LowerIdent,NoSpaceDotUpperIdent,KwAs,UpperIdent,
LowerIdent,OpAssign,OpenCurly,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotLowerIdent,
LowerIdent,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-import (raw "json.Json") (alias "MyModule"))
		(s-import (raw "http.Client") (alias "MyModule"))
		(s-decl
			(p-ident (raw "main"))
			(e-block
				(statements
					(s-decl
						(p-ident (raw "x"))
						(e-ident (raw "MyModule.parse")))
					(e-ident (raw "x")))))))
~~~
# FORMATTED
~~~roc
import json.Json as MyModule
import http.Client as MyModule

main = {
	x = MyModule.parse
	x
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "main"))
		(e-block
			(s-let
				(p-assign (ident "x"))
				(e-runtime-error (tag "ident_not_in_scope")))
			(e-lookup-local
				(p-assign (ident "x")))))
	(s-import (module "json.Json")
		(exposes))
	(s-import (module "http.Client")
		(exposes)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Error")))
	(expressions
		(expr (type "Error"))))
~~~
