# META
~~~ini
description=effectful_closure_statements
type=expr
~~~
# SOURCE
~~~roc
\{} ->
    echo "Welcome to the DMV!"
    age = readInt

    if age < 16 then
        echo "You're too young to drive!"
        exit 1
    else
        {}

    echo "Let's get started on your driver's license application."
~~~
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **\{** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**effectful_closure_statements.md:1:1:1:3:**
```roc
\{} ->
```
^^


# TOKENS
~~~zig
OpBackslash(1:1-1:2),OpenCurly(1:2-1:3),CloseCurly(1:3-1:4),OpArrow(1:5-1:7),Newline(1:1-1:1),
LowerIdent(2:5-2:9),StringStart(2:10-2:11),StringPart(2:11-2:30),StringEnd(2:30-2:31),Newline(1:1-1:1),
LowerIdent(3:5-3:8),OpAssign(3:9-3:10),LowerIdent(3:11-3:18),Newline(1:1-1:1),
Newline(1:1-1:1),
KwIf(5:5-5:7),LowerIdent(5:8-5:11),OpLessThan(5:12-5:13),Int(5:14-5:16),LowerIdent(5:17-5:21),Newline(1:1-1:1),
LowerIdent(6:9-6:13),StringStart(6:14-6:15),StringPart(6:15-6:41),StringEnd(6:41-6:42),Newline(1:1-1:1),
LowerIdent(7:9-7:13),Int(7:14-7:15),Newline(1:1-1:1),
KwElse(8:5-8:9),Newline(1:1-1:1),
OpenCurly(9:9-9:10),CloseCurly(9:10-9:11),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(11:5-11:9),StringStart(11:10-11:11),StringPart(11:11-11:66),StringEnd(11:66-11:67),EndOfFile(11:67-11:67),
~~~
# PARSE
~~~clojure
(e-malformed @1.1-1.3 (reason "expr_unexpected_token"))
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
