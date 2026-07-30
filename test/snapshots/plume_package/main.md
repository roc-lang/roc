# META
~~~ini
description=main mod from package
type=package
~~~
# SOURCE
~~~roc
package [
    Color,
] {}
~~~
# EXPECTED
MOD NOT FOUND - main.md:2:5:2:10
# PROBLEMS

┌──────────────────┐
│ MOD NOT FOUND ├─ The mod `Color` was not found in this Roc project. ──┐
└┬─────────────────┘                                                          │
 │                                                                            │
 │  Color,                                                                    │
 │  ‾‾‾‾‾                                                                     │
 └─────────────────────────────────────────────────────────────── main.md:2:5 ┘


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
(can-ir
	(s-import (mod "Color")
		(exposes)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(expressions))
~~~
