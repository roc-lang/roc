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
MODULE NOT FOUND - main.md:2:5:2:10
# PROBLEMS
                                                            ┌──────────────────┐
┌─ The module Color was not found in this Roc project. ─────┤ MODULE NOT FOUND │
│                                                           └─────────────────┬┘
│                                                                             │
│      Color,                                                                 │
│      ‾‾‾‾‾                                                                  │
└─────────────────────────────────────────────────────────────────────────────┘
    main.md:2:5

    You're attempting to use this module here:
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
	(s-import (module "Color")
		(exposes)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(expressions))
~~~
