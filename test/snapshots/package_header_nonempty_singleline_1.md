# META
~~~ini
description=package_header_nonempty_singleline (1)
type=file
~~~
# SOURCE
~~~roc
package [something, SomeType] { somePkg: "../main.roc", other: "../../other/main.roc" }
~~~
# EXPECTED
MODULE NOT FOUND - package_header_nonempty_singleline_1.md:1:21:1:29
EXPOSED BUT NOT DEFINED - package_header_nonempty_singleline_1.md:1:10:1:19
# PROBLEMS
                                                            ┌──────────────────┐
┌─ The module SomeType was not found in this Roc project. ──┤ MODULE NOT FOUND │
│                                                           └─────────────────┬┘
│                                                                             │
│  package [something, SomeType] { somePkg: "../main.roc", other: "../../other/main.roc" }│
│                      ‾‾‾‾‾‾‾‾                                               │
└─────────────────────────────────────────────────────────────────────────────┘
    package_header_nonempty_singleline_1.md:1:21

    You're attempting to use this module here:
                                                     ┌─────────────────────────┐
┌─ The module header says that something is ─────────┤ EXPOSED BUT NOT DEFINED │
│  exposed, but it is not defined anywhere in this   └────────────────────────┬┘
│  module.                                                                    │
│                                                                             │
│  package [something, SomeType] { somePkg: "../main.roc", other: "../../other/main.roc" }│
│           ‾‾‾‾‾‾‾‾‾                                                         │
└─────────────────────────────────────────────────────────────────────────────┘
    package_header_nonempty_singleline_1.md:1:10

    You can fix this by either defining something in this module, or by
    removing it from the list of exposed values.
# TOKENS
~~~zig
KwPackage,OpenSquare,LowerIdent,Comma,UpperIdent,CloseSquare,OpenCurly,LowerIdent,OpColon,StringStart,StringPart,StringEnd,Comma,LowerIdent,OpColon,StringStart,StringPart,StringEnd,CloseCurly,
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
					(e-string-part (raw "../main.roc"))))
			(record-field (name "other")
				(e-string
					(e-string-part (raw "../../other/main.roc"))))))
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
