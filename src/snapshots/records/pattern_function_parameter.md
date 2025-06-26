# META
~~~ini
description=Record pattern destructuring in function parameter
type=statement
~~~
# SOURCE
~~~roc
formatUser = |{ name, age, email }| "User: ${name} (${age.toStr()} years old) - Contact: ${email.display()}"
~~~
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: `expected_colon_after_pat_field_name`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**pattern_function_parameter.md:1:17:1:22:**
```roc
formatUser = |{ name, age, email }| "User: ${name} (${age.toStr()} years old) - Contact: ${email.display()}"
```


**UNEXPECTED TOKEN IN PATTERN**
The token **{ name** is not expected in a pattern.
Patterns can contain identifiers, literals, lists, records, or tags.

Here is the problematic code:
**pattern_function_parameter.md:1:15:1:21:**
```roc
formatUser = |{ name, age, email }| "User: ${name} (${age.toStr()} years old) - Contact: ${email.display()}"
```


**PARSE ERROR**
A parsing error occurred: `expected_expr_bar`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**pattern_function_parameter.md:1:37:1:44:**
```roc
formatUser = |{ name, age, email }| "User: ${name} (${age.toStr()} years old) - Contact: ${email.display()}"
```


# TOKENS
~~~zig
LowerIdent(1:1-1:11),OpAssign(1:12-1:13),OpBar(1:14-1:15),OpenCurly(1:15-1:16),LowerIdent(1:17-1:21),Comma(1:21-1:22),LowerIdent(1:23-1:26),Comma(1:26-1:27),LowerIdent(1:28-1:33),CloseCurly(1:34-1:35),OpBar(1:35-1:36),StringStart(1:37-1:38),StringPart(1:38-1:44),OpenStringInterpolation(1:44-1:46),LowerIdent(1:46-1:50),CloseStringInterpolation(1:50-1:51),StringPart(1:51-1:53),OpenStringInterpolation(1:53-1:55),LowerIdent(1:55-1:58),NoSpaceDotLowerIdent(1:58-1:64),NoSpaceOpenRound(1:64-1:65),CloseRound(1:65-1:66),CloseStringInterpolation(1:66-1:67),StringPart(1:67-1:90),OpenStringInterpolation(1:90-1:92),LowerIdent(1:92-1:97),NoSpaceDotLowerIdent(1:97-1:105),NoSpaceOpenRound(1:105-1:106),CloseRound(1:106-1:107),CloseStringInterpolation(1:107-1:108),StringPart(1:108-1:108),StringEnd(1:108-1:109),EndOfFile(1:109-1:109),
~~~
# PARSE
~~~clojure
(s-decl @1-1-1-44
	(p-ident @1-1-1-11 (raw "formatUser"))
	(e-malformed @1-37-1-44 (reason "expected_expr_bar")))
~~~
# FORMATTED
~~~roc
formatUser = 
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(expressions))
~~~