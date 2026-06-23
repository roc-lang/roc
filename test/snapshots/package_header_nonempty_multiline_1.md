# META
~~~ini
description=package_header_nonempty_multiline (1)
type=file
~~~
# SOURCE
~~~roc
package # This comment is here
	[something, SomeType]
	{ somePkg: "../main.roc" }
~~~
# EXPECTED
MODULE NOT FOUND - package_header_nonempty_multiline_1.md:2:14:2:22
EXPOSED BUT NOT DEFINED - package_header_nonempty_multiline_1.md:2:3:2:12
# PROBLEMS

┌──────────────────┐
│ MODULE NOT FOUND ├─ The module `SomeType` was not found in this Roc ────────┐
└┬─────────────────┘  project.                                                │
 │                                                                            │
 │  [something, SomeType]                                                     │
 │              ‾‾‾‾‾‾‾‾                                                      │
 └─────────────────────────────── package_header_nonempty_multiline_1.md:2:14 ┘



┌─────────────────────────┐
│ EXPOSED BUT NOT DEFINED ├─ The module header says that `something` is ──────┐
└┬────────────────────────┘  exposed, but it is not defined anywhere in       │
 │                           this module.                                     │
 │                                                                            │
 │  [something, SomeType]                                                     │
 │   ‾‾‾‾‾‾‾‾‾                                                                │
 └──────────────────────────────── package_header_nonempty_multiline_1.md:2:3 ┘

    You can fix this by either defining `something` in this module, or by
    removing it from the list of exposed values.

# TOKENS
~~~zig
KwPackage,
OpenSquare,LowerIdent,Comma,UpperIdent,CloseSquare,
OpenCurly,LowerIdent,OpColon,StringStart,StringPart,StringEnd,CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(package
		(exposes
			(exposed-lower-ident
				(text "something"))
			(exposed-upper-ident (text "SomeType")))
		(packages
			(record-field (name "somePkg")
				(e-string
					(e-string-part (raw "../main.roc"))))))
	(statements))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(s-import (module "SomeType")
		(exposes)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(expressions))
~~~
