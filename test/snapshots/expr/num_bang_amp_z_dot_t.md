# META
~~~ini
description=num_bang_amp_z_dot_t
type=snippet
~~~
# SOURCE
~~~roc
4
!
&z.t
~~~
# EXPECTED
PARSE ERROR - num_bang_amp_z_dot_t.md:1:1:1:2
PARSE ERROR - num_bang_amp_z_dot_t.md:2:1:2:2
PARSE ERROR - num_bang_amp_z_dot_t.md:3:1:3:2
PARSE ERROR - num_bang_amp_z_dot_t.md:3:2:3:3
PARSE ERROR - num_bang_amp_z_dot_t.md:3:3:3:5
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**num_bang_amp_z_dot_t.md:1:1:1:2:**
```roc
4
```
^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**num_bang_amp_z_dot_t.md:2:1:2:2:**
```roc
!
```
^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**num_bang_amp_z_dot_t.md:3:1:3:2:**
```roc
&z.t
```
^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**num_bang_amp_z_dot_t.md:3:2:3:3:**
```roc
&z.t
```
 ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**num_bang_amp_z_dot_t.md:3:3:3:5:**
```roc
&z.t
```
  ^^


# TOKENS
~~~zig
Int,
OpBang,
OpAmpersand,LowerIdent,NoSpaceDotLowerIdent,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))))
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
