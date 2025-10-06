# META
~~~ini
description=fuzz crash
type=file:FuzzCrash060.roc
~~~
# SOURCE
~~~roc
FuzzCrash060 := {}

0"
}
~~~
# EXPECTED
UNCLOSED STRING - :0:0:0:0
PARSE ERROR - fuzz_crash_060.md:3:1:3:2
PARSE ERROR - fuzz_crash_060.md:3:2:3:3
PARSE ERROR - fuzz_crash_060.md:3:3:3:3
PARSE ERROR - fuzz_crash_060.md:3:3:3:3
PARSE ERROR - fuzz_crash_060.md:4:1:4:2
# PROBLEMS
**UNCLOSED STRING**
This string is missing a closing quote.

```roc
0"
```
 ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_060.md:3:1:3:2:**
```roc
0"
```
^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_060.md:3:2:3:3:**
```roc
0"
```
 ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_060.md:3:3:3:3:**
```roc
0"
```
  ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_060.md:3:3:3:3:**
```roc
0"
```
  ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_060.md:4:1:4:2:**
```roc
}
```
^


# TOKENS
~~~zig
UpperIdent(1:1-1:13),OpColonEqual(1:14-1:16),OpenCurly(1:17-1:18),CloseCurly(1:18-1:19),
Int(3:1-3:2),StringStart(3:2-3:3),StringPart(3:3-3:3),StringEnd(3:3-3:3),
CloseCurly(4:1-4:2),
EndOfFile(5:1-5:1),
~~~
# PARSE
~~~clojure
(file @1.1-4.2
	(type-module @1.1-1.13)
	(statements
		(s-type-decl @1.1-1.19
			(header @1.1-1.13 (name "FuzzCrash060")
				(args))
			(ty-record @1.17-1.19))
		(s-malformed @3.1-3.2 (tag "statement_unexpected_token"))
		(s-malformed @3.2-3.3 (tag "statement_unexpected_token"))
		(s-malformed @3.3-3.3 (tag "statement_unexpected_token"))
		(s-malformed @3.3-3.3 (tag "statement_unexpected_token"))
		(s-malformed @4.1-4.2 (tag "statement_unexpected_token"))))
~~~
# FORMATTED
~~~roc
FuzzCrash060 := {}


~~~
# CANONICALIZE
~~~clojure
(can-ir
	(s-nominal-decl @1.1-1.19
		(ty-header @1.1-1.13 (name "FuzzCrash060"))
		(ty-record @1.17-1.19)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(type_decls
		(nominal @1.1-1.19 (type "FuzzCrash060")
			(ty-header @1.1-1.13 (name "FuzzCrash060"))))
	(expressions))
~~~
