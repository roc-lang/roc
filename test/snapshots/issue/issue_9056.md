# META
~~~ini
description=Formatter should preserve all module qualifiers in imports
type=snippet
~~~
# SOURCE
~~~roc
import a.B.C as D exposing [E]
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
KwImport,LowerIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,KwAs,UpperIdent,KwExposing,OpenSquare,UpperIdent,CloseSquare,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-import (raw "a.B.C") (alias "D")
			(exposing
				(exposed-upper-ident (text "E"))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(s-import (module "a.B")
		(exposes
			(exposed (name "E") (wildcard false)))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(expressions))
~~~
