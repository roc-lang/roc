# META
~~~ini
description=Multiline formatting app
type=file
~~~
# SOURCE
~~~roc
app [
	a1!,
	a2!,
] {
	pf: platform "../basic-cli/main.roc",
	a: "a",
}
~~~
# EXPECTED
EXPOSED BUT NOT DEFINED - app.md:3:2:3:5
EXPOSED BUT NOT DEFINED - app.md:2:2:2:5
# PROBLEMS
**EXPOSED BUT NOT DEFINED**
The module header says that `a2!` is exposed, but it is not defined anywhere in this module.

**app.md:3:2:3:5:**
```roc
	a2!,
```
	^^^
You can fix this by either defining `a2!` in this module, or by removing it from the list of exposed values.

**EXPOSED BUT NOT DEFINED**
The module header says that `a1!` is exposed, but it is not defined anywhere in this module.

**app.md:2:2:2:5:**
```roc
	a1!,
```
	^^^
You can fix this by either defining `a1!` in this module, or by removing it from the list of exposed values.

# TOKENS
~~~zig
KwApp(1:1-1:4),OpenSquare(1:5-1:6),
LowerIdent(2:2-2:5),Comma(2:5-2:6),
LowerIdent(3:2-3:5),Comma(3:5-3:6),
CloseSquare(4:1-4:2),OpenCurly(4:3-4:4),
LowerIdent(5:2-5:4),OpColon(5:4-5:5),KwPlatform(5:6-5:14),StringStart(5:15-5:16),StringPart(5:16-5:37),StringEnd(5:37-5:38),Comma(5:38-5:39),
LowerIdent(6:2-6:3),OpColon(6:3-6:4),StringStart(6:5-6:6),StringPart(6:6-6:7),StringEnd(6:7-6:8),Comma(6:8-6:9),
CloseCurly(7:1-7:2),EndOfFile(7:2-7:2),
~~~
# PARSE
~~~clojure
(file @1.1-7.2
	(app @1.1-7.2
		(provides @1.5-4.2
			(exposed-lower-ident @2.2-2.5
				(text "a1!"))
			(exposed-lower-ident @3.2-3.5
				(text "a2!")))
		(record-field @5.2-5.38 (name "pf")
			(e-string @5.15-5.38
				(e-string-part @5.16-5.37 (raw "../basic-cli/main.roc"))))
		(packages @4.3-7.2
			(record-field @5.2-5.38 (name "pf")
				(e-string @5.15-5.38
					(e-string-part @5.16-5.37 (raw "../basic-cli/main.roc"))))
			(record-field @6.2-6.8 (name "a")
				(e-string @6.5-6.8
					(e-string-part @6.6-6.7 (raw "a"))))))
	(statements))
~~~
# FORMATTED
~~~roc
NO CHANGE
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
