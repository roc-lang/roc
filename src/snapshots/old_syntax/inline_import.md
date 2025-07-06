# META
~~~ini
description=inline_import
type=expr
~~~
# SOURCE
~~~roc
import Json exposing [int]
import Json.Encode as JE

JE.encode (int 42)
~~~
# EXPECTED
UNEXPECTED TOKEN IN EXPRESSION - inline_import.md:1:1:1:12
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **import Json** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**inline_import.md:1:1:1:12:**
```roc
import Json exposing [int]
```
^^^^^^^^^^^


# TOKENS
~~~zig
KwImport(1:1-1:7),UpperIdent(1:8-1:12),KwExposing(1:13-1:21),OpenSquare(1:22-1:23),LowerIdent(1:23-1:26),CloseSquare(1:26-1:27),Newline(1:1-1:1),
KwImport(2:1-2:7),UpperIdent(2:8-2:12),NoSpaceDotUpperIdent(2:12-2:19),KwAs(2:20-2:22),UpperIdent(2:23-2:25),Newline(1:1-1:1),
Newline(1:1-1:1),
UpperIdent(4:1-4:3),NoSpaceDotLowerIdent(4:3-4:10),OpenRound(4:11-4:12),LowerIdent(4:12-4:15),Int(4:16-4:18),CloseRound(4:18-4:19),EndOfFile(4:19-4:19),
~~~
# PARSE
~~~clojure
(e-malformed @1.1-1.12 (reason "expr_unexpected_token"))
~~~
# FORMATTED
~~~roc

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
