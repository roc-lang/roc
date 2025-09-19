# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
*import B as
~~~
# EXPECTED
MISSING HEADER - fuzz_crash_038.md:1:1:1:2
PARSE ERROR - fuzz_crash_038.md:1:2:1:8
# PROBLEMS
**MISSING HEADER**
Roc files must start with a module header.

For example:
        module [main]
or for an app:
        app [main!] { pf: platform "../basic-cli/platform.roc" }

**fuzz_crash_038.md:1:1:1:2:**
```roc
*import B as
```
^


**PARSE ERROR**
A parsing error occurred: `expected_upper_name_after_import_as`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_038.md:1:2:1:8:**
```roc
*import B as
```
 ^^^^^^


# TOKENS
~~~zig
OpStar(1:1-1:2),KwImport(1:2-1:8),UpperIdent(1:9-1:10),KwAs(1:11-1:13),
EndOfFile(2:1-2:1),
~~~
# PARSE
~~~clojure
(file @1.1-1.13
	(malformed-header @1.1-1.2 (tag "missing_header"))
	(statements
		(s-malformed @1.2-1.13 (tag "expected_upper_name_after_import_as"))))
~~~
# FORMATTED
~~~roc
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(s-nominal-decl @1.1-1.1
		(ty-header @1.1-1.1 (name "Bool"))
		(ty-tag-union @1.1-1.1
			(tag_name @1.1-1.1 (name "True"))
			(tag_name @1.1-1.1 (name "False"))))
	(s-nominal-decl @1.1-1.1
		(ty-header @1.1-1.1 (name "Result")
			(ty-args
				(ty-rigid-var @1.1-1.1 (name "ok"))
				(ty-rigid-var @1.1-1.1 (name "err"))))
		(ty-tag-union @1.1-1.1
			(tag_name @1.1-1.1 (name "Ok"))
			(tag_name @1.1-1.1 (name "Err")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(type_decls
		(nominal @1.1-1.1 (type "Bool")
			(ty-header @1.1-1.1 (name "Bool")))
		(nominal @1.1-1.1 (type "Result(ok, err)")
			(ty-header @1.1-1.1 (name "Result")
				(ty-args
					(ty-rigid-var @1.1-1.1 (name "ok"))
					(ty-rigid-var @1.1-1.1 (name "err"))))))
	(expressions))
~~~
