# META
~~~ini
description=Issue 9373 - Formatter must keep the imported alias on the same line as `as`, even with multiline qualifiers
type=snippet
~~~
# SOURCE
~~~roc
import A .B as
    X1
import A .B .C as
    X2
import A .B .C .D as
    X3
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
KwImport,UpperIdent,DotUpperIdent,KwAs,
UpperIdent,
KwImport,UpperIdent,DotUpperIdent,DotUpperIdent,KwAs,
UpperIdent,
KwImport,UpperIdent,DotUpperIdent,DotUpperIdent,DotUpperIdent,KwAs,
UpperIdent,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-import (raw "A .B") (alias "X1"))
		(s-import (raw "A .B .C") (alias "X2"))
		(s-import (raw "A .B .C .D") (alias "X3"))))
~~~
# FORMATTED
~~~roc
import A
	.B as X1 exposing []
import A
	.B
	.C as X2 exposing []
import A
	.B
	.C
	.D as X3 exposing []
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(s-import (module "A")
		(exposes))
	(s-import (module "A")
		(exposes))
	(s-import (module "A")
		(exposes)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(expressions))
~~~
