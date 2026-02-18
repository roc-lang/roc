# META
~~~ini
description=main module from package
type=package
~~~
# SOURCE
~~~roc
package [
    Color,
] {}
~~~
# EXPECTED
EXPOSED BUT NOT DEFINED - main.md:2:5:2:10
# PROBLEMS
**EXPOSED BUT NOT DEFINED**
The module header says that `Color` is exposed, but it is not defined anywhere in this module.

**main.md:2:5:2:10:**
```roc
    Color,
```
    ^^^^^
You can fix this by either defining `Color` in this module, or by removing it from the list of exposed values.

# TOKENS
~~~zig
KwPackage,OpenSquare,
UpperIdent,Comma,
CloseSquare,OpenCurly,CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(package
		(exposes
			(exposed-upper-ident (text "Color")))
		(packages))
	(statements))
~~~
# FORMATTED
~~~roc
package
	[
		Color,
	]
	{}
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
