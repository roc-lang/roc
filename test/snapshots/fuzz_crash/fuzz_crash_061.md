# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
platform"
requires{}{n:0[import S	exposing[
~~~
# TOKENS
~~~text
KwPlatform MalformedString KwRequires OpenCurly CloseCurly OpenCurly LowerIdent OpColon Int OpenSquare KwImport UpperIdent KwExposing OpenSquare ~~~
# PARSE
~~~clojure
(platform-header
  (exposes
    (malformed)
))
(block
  (list_literal)
)
~~~
# FORMATTED
~~~roc
platform "
 requires { n: 0 } exposes [import ]

[]
~~~
# EXPECTED
UNCLOSED STRING - :0:0:0:0
UNEXPECTED TOKEN IN TYPE ANNOTATION - fuzz_crash_061.md:2:14:2:15
PARSE ERROR - fuzz_crash_061.md:2:11:2:12
PARSE ERROR - fuzz_crash_061.md:2:16:2:22
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **"
** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_061.md:1:9:2:1:**
```roc
platform"
requires{}{n:0[import S	exposing[
```


**PARSE ERROR**
A parsing error occurred: **expected_requires_signatures_close_curly**
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_061.md:1:1:2:15:**
```roc
platform"
requires{}{n:0[import S	exposing[
```


**EXPECTED EXPOSES**
Module headers must have an `exposing` section that lists what the module exposes.
For example:     module [main, add, subtract]

**fuzz_crash_061.md:1:1:2:15:**
```roc
platform"
requires{}{n:0[import S	exposing[
```


**PARSE ERROR**
A parsing error occurred: **exposed_item_unexpected_token**
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_061.md:2:16:2:23:**
```roc
requires{}{n:0[import S	exposing[
```
               ^^^^^^^


**EXPECTED CLOSING BRACKET**
Module headers must have an `exposing` section that lists what the module exposes.
For example:     module [main, add, subtract]

**fuzz_crash_061.md:1:1:2:23:**
```roc
platform"
requires{}{n:0[import S	exposing[
```


**EXPECTED PACKAGES**
A parsing error occurred: **expected_packages**
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_061.md:1:1:2:23:**
```roc
platform"
requires{}{n:0[import S	exposing[
```


**PARSE ERROR**
A parsing error occurred: **expected_packages_open_curly**
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_061.md:1:1:2:23:**
```roc
platform"
requires{}{n:0[import S	exposing[
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **S	** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_061.md:2:23:2:25:**
```roc
requires{}{n:0[import S	exposing[
```
                      ^^


**EXPECTED CLOSE CURLY BRACE**
A parsing error occurred: **expected_packages_close_curly**
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_061.md:1:1:2:25:**
```roc
platform"
requires{}{n:0[import S	exposing[
```


**PARSE ERROR**
A parsing error occurred: **expected_provides**
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_061.md:1:1:2:25:**
```roc
platform"
requires{}{n:0[import S	exposing[
```


**PARSE ERROR**
A parsing error occurred: **expected_provides_open_square**
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_061.md:1:1:2:25:**
```roc
platform"
requires{}{n:0[import S	exposing[
```


**PARSE ERROR**
A parsing error occurred: **exposed_item_unexpected_token**
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_061.md:2:25:2:33:**
```roc
requires{}{n:0[import S	exposing[
```
                       	^^^^^^^^


**PARSE ERROR**
A parsing error occurred: **expected_provides_close_square**
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_061.md:1:1:2:33:**
```roc
platform"
requires{}{n:0[import S	exposing[
```


**LIST NOT CLOSED**
This list is not properly closed.
Expected either a comma **,** to continue the list or a closing bracket **]** to end it.

**fuzz_crash_061.md:2:33:2:34:**
```roc
requires{}{n:0[import S	exposing[
```
                       	        ^


# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.list_literal)
)
~~~
# SOLVED
~~~clojure
; Total type variables: 13
(var #0 _)
(var #1 _)
(var #2 _)
(var #3 _)
(var #4 _)
(var #5 _)
(var #6 _)
(var #7 _)
(var #8 _)
(var #9 -> #12)
(var #10 _)
(var #11 _)
(var #12 List #11)
~~~
# TYPES
~~~roc
~~~
