# META
~~~ini
description=Import with exposing clause using aliases
type=file
~~~
# SOURCE
~~~roc
module [main]

import json.Json exposing [decode as fromJson, encode as toJson]

main = {
	data = { name: "Bob", age: 25 }
	encoded = toJson(data)
	decoded = fromJson(encoded)
	decoded
}
~~~
# TOKENS
~~~text
KwModule OpenSquare LowerIdent CloseSquare BlankLine KwImport LowerIdent Dot UpperIdent KwExposing OpenSquare LowerIdent KwAs LowerIdent Comma LowerIdent KwAs LowerIdent CloseSquare BlankLine LowerIdent OpAssign OpenCurly LowerIdent OpAssign OpenCurly LowerIdent OpColon String Comma LowerIdent OpColon Int CloseCurly LowerIdent OpAssign LowerIdent OpenRound LowerIdent CloseRound LowerIdent OpAssign LowerIdent OpenRound LowerIdent CloseRound LowerIdent CloseCurly ~~~
# PARSE
~~~clojure
(module-header
  (exposes
    (lc "main")
))
~~~
# FORMATTED
~~~roc
module [main]


import json.Json exposing [decode]
as 
fromJson
, 
encode
as 
toJson
]

main = {
	data = { name : "Bob", age : 25 }
	encoded = toJson(data)
	decoded = fromJson(encoded)
	decoded
}
~~~
# EXPECTED
NIL
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **as ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**import_exposing_alias.md:3:35:3:38:**
```roc
import json.Json exposing [decode as fromJson, encode as toJson]
```
                                  ^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **, ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**import_exposing_alias.md:3:46:3:48:**
```roc
import json.Json exposing [decode as fromJson, encode as toJson]
```
                                             ^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **as ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**import_exposing_alias.md:3:55:3:58:**
```roc
import json.Json exposing [decode as fromJson, encode as toJson]
```
                                                      ^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **]

** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**import_exposing_alias.md:3:64:5:1:**
```roc
import json.Json exposing [decode as fromJson, encode as toJson]

main = {
```


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**import_exposing_alias.md:3:35:3:38:**
```roc
import json.Json exposing [decode as fromJson, encode as toJson]
```
                                  ^^^


**EXPRESSION IN STATEMENT CONTEXT**
Found an expression where a statement was expected.
This might be a missing semicolon or an incorrectly placed expression.

**import_exposing_alias.md:3:38:3:46:**
```roc
import json.Json exposing [decode as fromJson, encode as toJson]
```
                                     ^^^^^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**import_exposing_alias.md:3:46:3:48:**
```roc
import json.Json exposing [decode as fromJson, encode as toJson]
```
                                             ^^


**EXPRESSION IN STATEMENT CONTEXT**
Found an expression where a statement was expected.
This might be a missing semicolon or an incorrectly placed expression.

**import_exposing_alias.md:3:48:3:54:**
```roc
import json.Json exposing [decode as fromJson, encode as toJson]
```
                                               ^^^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**import_exposing_alias.md:3:55:3:58:**
```roc
import json.Json exposing [decode as fromJson, encode as toJson]
```
                                                      ^^^


**EXPRESSION IN STATEMENT CONTEXT**
Found an expression where a statement was expected.
This might be a missing semicolon or an incorrectly placed expression.

**import_exposing_alias.md:3:58:3:64:**
```roc
import json.Json exposing [decode as fromJson, encode as toJson]
```
                                                         ^^^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**import_exposing_alias.md:3:64:5:1:**
```roc
import json.Json exposing [decode as fromJson, encode as toJson]

main = {
```


**TYPE IN EXPRESSION CONTEXT**
Found a type annotation where an expression was expected.
Type annotations should appear after a colon in declarations, not in expression contexts.

**import_exposing_alias.md:6:11:6:22:**
```roc
	data = { name: "Bob", age: 25 }
```
	         ^^^^^^^^^^^


**TYPE IN EXPRESSION CONTEXT**
Found a type annotation where an expression was expected.
Type annotations should appear after a colon in declarations, not in expression contexts.

**import_exposing_alias.md:6:24:6:31:**
```roc
	data = { name: "Bob", age: 25 }
```
	                      ^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named **toJson** in this scope.
Is there an **import** or **exposing** missing up-top?

**import_exposing_alias.md:7:12:7:18:**
```roc
	encoded = toJson(data)
```
	          ^^^^^^


**UNDEFINED VARIABLE**
Nothing is named **fromJson** in this scope.
Is there an **import** or **exposing** missing up-top?

**import_exposing_alias.md:8:12:8:20:**
```roc
	decoded = fromJson(encoded)
```
	          ^^^^^^^^


# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
~~~
