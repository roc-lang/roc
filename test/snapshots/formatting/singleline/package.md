# META
~~~ini
description=Singleline formatting package
type=file
~~~
# SOURCE
~~~roc
package [a!, b!] { a: "a", b: "b" }

a! : Str => Str
b! : Str => Str
~~~
# EXPECTED
EXPOSED BUT NOT DEFINED - package.md:1:10:1:12
EXPOSED BUT NOT DEFINED - package.md:1:14:1:16
# PROBLEMS
**EXPOSED BUT NOT DEFINED**
The module header says that `a!` is exposed, but it is not defined anywhere in this module.

**package.md:1:10:1:12:**
```roc
package [a!, b!] { a: "a", b: "b" }
```
         ^^
You can fix this by either defining `a!` in this module, or by removing it from the list of exposed values.

**EXPOSED BUT NOT DEFINED**
The module header says that `b!` is exposed, but it is not defined anywhere in this module.

**package.md:1:14:1:16:**
```roc
package [a!, b!] { a: "a", b: "b" }
```
             ^^
You can fix this by either defining `b!` in this module, or by removing it from the list of exposed values.

# TOKENS
~~~zig
KwPackage(1:1-1:8),OpenSquare(1:9-1:10),LowerIdent(1:10-1:12),Comma(1:12-1:13),LowerIdent(1:14-1:16),CloseSquare(1:16-1:17),OpenCurly(1:18-1:19),LowerIdent(1:20-1:21),OpColon(1:21-1:22),StringStart(1:23-1:24),StringPart(1:24-1:25),StringEnd(1:25-1:26),Comma(1:26-1:27),LowerIdent(1:28-1:29),OpColon(1:29-1:30),StringStart(1:31-1:32),StringPart(1:32-1:33),StringEnd(1:33-1:34),CloseCurly(1:35-1:36),
LowerIdent(3:1-3:3),OpColon(3:4-3:5),UpperIdent(3:6-3:9),OpFatArrow(3:10-3:12),UpperIdent(3:13-3:16),
LowerIdent(4:1-4:3),OpColon(4:4-4:5),UpperIdent(4:6-4:9),OpFatArrow(4:10-4:12),UpperIdent(4:13-4:16),
EndOfFile(5:1-5:1),
~~~
# PARSE
~~~clojure
(file @1.1-4.16
	(package @1.1-1.36
		(exposes @1.9-1.17
			(exposed-lower-ident @1.10-1.12
				(text "a!"))
			(exposed-lower-ident @1.14-1.16
				(text "b!")))
		(packages @1.18-1.36
			(record-field @1.20-1.26 (name "a")
				(e-string @1.23-1.26
					(e-string-part @1.24-1.25 (raw "a"))))
			(record-field @1.28-1.34 (name "b")
				(e-string @1.31-1.34
					(e-string-part @1.32-1.33 (raw "b"))))))
	(statements
		(s-type-anno @3.1-3.16 (name "a!")
			(ty-fn @3.6-3.16
				(ty @3.6-3.9 (name "Str"))
				(ty @3.13-3.16 (name "Str"))))
		(s-type-anno @4.1-4.16 (name "b!")
			(ty-fn @4.6-4.16
				(ty @4.6-4.9 (name "Str"))
				(ty @4.13-4.16 (name "Str"))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
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
