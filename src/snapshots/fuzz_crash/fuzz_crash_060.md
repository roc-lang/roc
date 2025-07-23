# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
module[]C:k||match 0{0|#
0"
}
~~~
# EXPECTED
UNCLOSED STRING - :0:0:0:0
PARSE ERROR - fuzz_crash_060.md:2:2:2:2
COMPILER DIAGNOSTIC - fuzz_crash_060.md:0:0:0:0
COMPILER DIAGNOSTIC - fuzz_crash_060.md:0:0:0:0
# PROBLEMS
**UNCLOSED STRING**
This string is missing a closing quote.

**PARSE ERROR**
A parsing error occurred: `match_branch_missing_arrow`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**fuzz_crash_060.md:2:2:2:2:**
```roc
0"
```
 


**COMPILER DIAGNOSTIC**

**Compiler Diagnostic**
Diagnostic type 'undeclared_type_var' is not yet handled in report generation.
**fuzz_crash_060.md:0:0:0:0**

**COMPILER DIAGNOSTIC**

**Compiler Diagnostic**
Diagnostic type 'invalid_top_level_statement' is not yet handled in report generation.
**fuzz_crash_060.md:0:0:0:0**

# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:7-1:8),CloseSquare(1:8-1:9),UpperIdent(1:9-1:10),OpColon(1:10-1:11),LowerIdent(1:11-1:12),OpBar(1:12-1:13),OpBar(1:13-1:14),KwMatch(1:14-1:19),Int(1:20-1:21),OpenCurly(1:21-1:22),Int(1:22-1:23),OpBar(1:23-1:24),
Int(2:1-2:2),StringStart(2:2-2:3),StringPart(2:3-2:3),StringEnd(2:3-2:3),
CloseCurly(3:1-3:2),EndOfFile(3:2-3:2),
~~~
# PARSE
~~~clojure
(file @1.1-3.2
	(module @1.1-1.9
		(exposes @1.7-1.9))
	(statements
		(s-type-decl @1.9-1.12
			(header @1.9-1.10 (name "C")
				(args))
			(ty-var @1.11-1.12 (raw "k")))
		(e-lambda @1.12-3.2
			(args)
			(e-match
				(e-int @1.20-1.21 (raw "0"))
				(branches
					(branch @1.22-2.3
						(p-alternatives
							(p-int @1.22-1.23 (raw "0"))
							(p-int @2.1-2.2 (raw "0")))
						(e-string @2.2-2.3
							(e-string-part @2.3-2.3 (raw "")))))))))
~~~
# FORMATTED
~~~roc
module []
C : k
|| match 0 {
	0
	|
		0 => ""
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(s-alias-decl @1.9-1.12
		(type-header (name "C"))
		(ty-var @1.11-1.12 (name "k"))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(type_decls
		(alias @1.9-1.12 (type "C")
			(type-header (name "C"))))
	(expressions))
~~~
