# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
app[]{f:platform""}{
o:0}0
~~~
# EXPECTED
PARSE ERROR - fuzz_crash_043.md:1:20:1:21
UNEXPECTED TOKEN IN TYPE ANNOTATION - fuzz_crash_043.md:2:3:2:4
UNEXPECTED TOKEN IN EXPRESSION - fuzz_crash_043.md:2:4:2:5
MALFORMED TYPE - fuzz_crash_043.md:2:3:2:4
INVALID STATEMENT - fuzz_crash_043.md:2:4:2:5
INVALID STATEMENT - fuzz_crash_043.md:2:5:2:6
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**fuzz_crash_043.md:1:20:1:21:**
```roc
app[]{f:platform""}{
```
                   ^


**UNEXPECTED TOKEN IN TYPE ANNOTATION**
The token **0** is not expected in a type annotation.
Type annotations should contain types like _Str_, _Num a_, or _List U64_.

Here is the problematic code:
**fuzz_crash_043.md:2:3:2:4:**
```roc
o:0}0
```
  ^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **}** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**fuzz_crash_043.md:2:4:2:5:**
```roc
o:0}0
```
   ^


**MALFORMED TYPE**
This type annotation is malformed or contains invalid syntax.

**fuzz_crash_043.md:2:3:2:4:**
```roc
o:0}0
```
  ^


**INVALID STATEMENT**
The statement `expression` is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**fuzz_crash_043.md:2:4:2:5:**
```roc
o:0}0
```
   ^


**INVALID STATEMENT**
The statement `expression` is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**fuzz_crash_043.md:2:5:2:6:**
```roc
o:0}0
```
    ^


# TOKENS
~~~zig
KwApp(1:1-1:4),OpenSquare(1:4-1:5),CloseSquare(1:5-1:6),OpenCurly(1:6-1:7),LowerIdent(1:7-1:8),OpColon(1:8-1:9),KwPlatform(1:9-1:17),StringStart(1:17-1:18),StringPart(1:18-1:18),StringEnd(1:18-1:19),CloseCurly(1:19-1:20),OpenCurly(1:20-1:21),
LowerIdent(2:1-2:2),OpColon(2:2-2:3),Int(2:3-2:4),CloseCurly(2:4-2:5),Int(2:5-2:6),EndOfFile(2:6-2:6),
~~~
# PARSE
~~~clojure
(file @1.1-2.6
	(app @1.1-1.20
		(provides @1.4-1.6)
		(record-field @1.7-1.19 (name "f")
			(e-string @1.17-1.19
				(e-string-part @1.18-1.18 (raw ""))))
		(packages @1.6-1.20
			(record-field @1.7-1.19 (name "f")
				(e-string @1.17-1.19
					(e-string-part @1.18-1.18 (raw ""))))))
	(statements
		(s-malformed @1.20-1.21 (tag "statement_unexpected_token"))
		(s-type-anno @2.1-2.4 (name "o")
			(ty-malformed @2.3-2.4 (tag "ty_anno_unexpected_token")))
		(e-malformed @2.4-2.5 (reason "expr_unexpected_token"))
		(e-int @2.5-2.6 (raw "0"))))
~~~
# FORMATTED
~~~roc
app [] { f: platform "" }

o : 
0
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
