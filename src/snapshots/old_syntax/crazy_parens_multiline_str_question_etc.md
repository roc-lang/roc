# META
~~~ini
description=crazy_parens_multiline_str_question_etc
type=expr
~~~
# SOURCE
~~~roc
((d
-""""""()?))Y
~~~
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: `expected_expr_close_round_or_comma`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**crazy_parens_multiline_str_question_etc.md:2:9:2:11:**
```roc
-""""""()?))Y
```
        ^^


**PARSE ERROR**
A parsing error occurred: `expected_expr_close_round_or_comma`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**crazy_parens_multiline_str_question_etc.md:2:11:2:13:**
```roc
-""""""()?))Y
```
          ^^


# TOKENS
~~~zig
OpenRound(1:1-1:2),NoSpaceOpenRound(1:2-1:3),LowerIdent(1:3-1:4),Newline(1:1-1:1),
OpUnaryMinus(2:1-2:2),MultilineStringStart(2:2-2:5),StringPart(2:5-2:5),MultilineStringEnd(2:5-2:8),NoSpaceOpenRound(2:8-2:9),CloseRound(2:9-2:10),NoSpaceOpQuestion(2:10-2:11),CloseRound(2:11-2:12),CloseRound(2:12-2:13),UpperIdent(2:13-2:14),EndOfFile(2:14-2:14),
~~~
# PARSE
~~~clojure
(e-malformed @2.11-2.13 (reason "expected_expr_close_round_or_comma"))
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
