# META
~~~ini
description=Expected an open bracket for the header
type=file
~~~
# SOURCE
~~~roc
module
~~~
# TOKENS
~~~text
KwModule ~~~
# PARSE
~~~clojure
(module-header
  (exposes))
~~~
# FORMATTED
~~~roc
module []
~~~
# EXPECTED
PARSE ERROR - header_expected_open_bracket.md:2:1:2:1
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: **header_expected_open_square**
This is an unexpected parsing error. Please check your syntax.

**header_expected_open_bracket.md:1:1:1:7:**
```roc
module
```
^^^^^^


# CANONICALIZE
~~~clojure
(empty)
~~~
# SOLVED
~~~clojure
~~~
# TYPES
~~~roc
~~~
