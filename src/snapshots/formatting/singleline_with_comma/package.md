# META
~~~ini
description=Singleline with comma formatting package
type=file
~~~
# SOURCE
~~~roc
package [a!, b!,] { a: "a", b: "b", }

a! : Str => Str
b! : Str => Str
~~~
# EXPECTED
EXPOSED BUT NOT DEFINED - package.md:1:10:1:12
EXPOSED BUT NOT DEFINED - package.md:1:14:1:16
# PROBLEMS
**EXPOSED BUT NOT DEFINED**

**Exposed but Not Defined**
'a!' is exposed in the module header but is not defined:
**package.md:1:10:1:12:**
```roc
package [a!, b!,] { a: "a", b: "b", }
```
         ^^


**EXPOSED BUT NOT DEFINED**

**Exposed but Not Defined**
'b!' is exposed in the module header but is not defined:
**package.md:1:14:1:16:**
```roc
package [a!, b!,] { a: "a", b: "b", }
```
             ^^


# TOKENS
~~~zig
KwPackage(1:1-1:8),OpenSquare(1:9-1:10),LowerIdent(1:10-1:12),Comma(1:12-1:13),LowerIdent(1:14-1:16),Comma(1:16-1:17),CloseSquare(1:17-1:18),OpenCurly(1:19-1:20),LowerIdent(1:21-1:22),OpColon(1:22-1:23),StringStart(1:24-1:25),StringPart(1:25-1:26),StringEnd(1:26-1:27),Comma(1:27-1:28),LowerIdent(1:29-1:30),OpColon(1:30-1:31),StringStart(1:32-1:33),StringPart(1:33-1:34),StringEnd(1:34-1:35),Comma(1:35-1:36),CloseCurly(1:37-1:38),
LowerIdent(3:1-3:3),OpColon(3:4-3:5),UpperIdent(3:6-3:9),OpFatArrow(3:10-3:12),UpperIdent(3:13-3:16),
LowerIdent(4:1-4:3),OpColon(4:4-4:5),UpperIdent(4:6-4:9),OpFatArrow(4:10-4:12),UpperIdent(4:13-4:16),EndOfFile(4:16-4:16),
~~~
# PARSE
~~~clojure
(file @1.1-4.16
	(package @1.1-1.38
		(exposes @1.9-1.18
			(exposed-lower-ident @1.10-1.12
				(text "a!"))
			(exposed-lower-ident @1.14-1.16
				(text "b!")))
		(packages @1.19-1.38
			(record-field @1.21-1.27 (name "a")
				(e-string @1.24-1.27
					(e-string-part @1.25-1.26 (raw "a"))))
			(record-field @1.29-1.35 (name "b")
				(e-string @1.32-1.35
					(e-string-part @1.33-1.34 (raw "b"))))))
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
package
	[
		a!,
		b!,
	]
	{
		a: "a",
		b: "b",
	}

a! : Str => Str
b! : Str => Str
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
