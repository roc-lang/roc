# META
~~~ini
description=package_header_nonempty_multiline (3)
type=file
~~~
# SOURCE
~~~roc
package
	[something, SomeType,]
	{ somePkg: "../main.roc", }
~~~
# EXPECTED
MOD NOT FOUND - package_header_nonempty_multiline_3.md:2:14:2:22
EXPOSED BUT NOT DEFINED - package_header_nonempty_multiline_3.md:2:3:2:12
# PROBLEMS

┌──────────────────┐
│ MOD NOT FOUND ├─ The mod `SomeType` was not found in this Roc ────────┐
└┬─────────────────┘  project.                                                │
 │                                                                            │
 │  [something, SomeType,]                                                    │
 │              ‾‾‾‾‾‾‾‾                                                      │
 └─────────────────────────────── package_header_nonempty_multiline_3.md:2:14 ┘



┌─────────────────────────┐
│ EXPOSED BUT NOT DEFINED ├─ The mod header says that `something` is ──────┐
└┬────────────────────────┘  exposed, but it is not defined anywhere in       │
 │                           this mod.                                     │
 │                                                                            │
 │  [something, SomeType,]                                                    │
 │   ‾‾‾‾‾‾‾‾‾                                                                │
 └──────────────────────────────── package_header_nonempty_multiline_3.md:2:3 ┘

    You can fix this by either defining `something` in this mod, or by
    removing it from the list of exposed values.

# TOKENS
~~~zig
KwPackage,
OpenSquare,LowerIdent,Comma,UpperIdent,Comma,CloseSquare,
OpenCurly,LowerIdent,OpColon,StringStart,StringPart,StringEnd,Comma,CloseCurly,
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
package
	[
		something,
		SomeType,
	]
	{
		somePkg: "../main.roc",
	}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(s-import (mod "SomeType")
		(exposes)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(expressions))
~~~
