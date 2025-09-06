# META
~~~ini
description=A primitive
type=file
~~~
# SOURCE
~~~roc
module []

import json.Json [foo, BAR]
~~~
# TOKENS
~~~text
KwModule OpenSquare CloseSquare BlankLine KwImport LowerIdent Dot UpperIdent OpenSquare LowerIdent Comma UpperIdent CloseSquare ~~~
# PARSE
~~~clojure
(module-header)
~~~
# FORMATTED
~~~roc
module []

import json.Json
[foo, BAR]
~~~
# EXPECTED
PARSE ERROR - stmt_import.md:3:18:3:19
PARSE ERROR - stmt_import.md:3:19:3:22
PARSE ERROR - stmt_import.md:3:22:3:23
PARSE ERROR - stmt_import.md:3:27:3:28
MODULE NOT FOUND - stmt_import.md:3:1:3:17
# PROBLEMS
**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**stmt_import.md:3:18:3:28:**
```roc
import json.Json [foo, BAR]
```
                 ^^^^^^^^^^


# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.import)
  (Stmt.malformed)
)
~~~
# SOLVED
~~~clojure
~~~
# TYPES
~~~roc
~~~
