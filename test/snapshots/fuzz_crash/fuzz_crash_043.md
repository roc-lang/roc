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
PARSE ERROR - fuzz_crash_043.md:2:4:2:5
PARSE ERROR - fuzz_crash_043.md:2:5:2:6
MALFORMED TYPE - fuzz_crash_043.md:2:3:2:4
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_043.md:1:20:1:21:**
```roc
app[]{f:platform""}{
```
                   ^


**UNEXPECTED TOKEN IN TYPE ANNOTATION**
The token **0** is not expected in a type annotation.
Type annotations should contain types like _Str_, _Num a_, or _List U64_.

**fuzz_crash_043.md:2:3:2:4:**
```roc
o:0}0
```
  ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_043.md:2:4:2:5:**
```roc
o:0}0
```
   ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_043.md:2:5:2:6:**
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


# TOKENS
~~~zig
KwApp,OpenSquare,CloseSquare,OpenCurly,LowerIdent,OpColon,KwPlatform,StringStart,StringPart,StringEnd,CloseCurly,OpenCurly,
LowerIdent,OpColon,Int,CloseCurly,Int,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(app
		(provides)
		(record-field (name "f")
			(e-string
				(e-string-part (raw ""))))
		(packages
			(record-field (name "f")
				(e-string
					(e-string-part (raw ""))))))
	(statements
		(s-malformed (tag "statement_unexpected_token"))
		(s-type-anno (name "o")
			(ty-malformed (tag "ty_anno_unexpected_token")))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))))
~~~
# FORMATTED
~~~roc
app [] { f: platform "" }

o : 
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(s-type-anno (name "o")
		(ty-malformed)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(expressions))
~~~
