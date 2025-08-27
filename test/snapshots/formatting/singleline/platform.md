# META
~~~ini
description=Singleline formatting platform
type=file
~~~
# SOURCE
~~~roc
platform "pf"
	requires { R1, R2 } { r1 : R1 -> R2, r2 : R1 -> R2 }
	exposes [E1, E2]
	packages { pa1: "pa1", pa2: "pa2" }
	# imports [I1.{ I11, I12 }, I2.{ I21, I22 }]
	provides { pr1: "not implemented", pr2: "not implemented" }
~~~
# EXPECTED
EXPOSED BUT NOT DEFINED - platform.md:3:11:3:13
EXPOSED BUT NOT DEFINED - platform.md:3:15:3:17
# PROBLEMS
**EXPOSED BUT NOT DEFINED**
The module header says that `E1` is exposed, but it is not defined anywhere in this module.

**platform.md:3:11:3:13:**
```roc
	exposes [E1, E2]
```
	         ^^
You can fix this by either defining `E1` in this module, or by removing it from the list of exposed values.

**EXPOSED BUT NOT DEFINED**
The module header says that `E2` is exposed, but it is not defined anywhere in this module.

**platform.md:3:15:3:17:**
```roc
	exposes [E1, E2]
```
	             ^^
You can fix this by either defining `E2` in this module, or by removing it from the list of exposed values.

# TOKENS
~~~zig
KwPlatform(1:1-1:9),StringStart(1:10-1:11),StringPart(1:11-1:13),StringEnd(1:13-1:14),
KwRequires(2:2-2:10),OpenCurly(2:11-2:12),UpperIdent(2:13-2:15),Comma(2:15-2:16),UpperIdent(2:17-2:19),CloseCurly(2:20-2:21),OpenCurly(2:22-2:23),LowerIdent(2:24-2:26),OpColon(2:27-2:28),UpperIdent(2:29-2:31),OpArrow(2:32-2:34),UpperIdent(2:35-2:37),Comma(2:37-2:38),LowerIdent(2:39-2:41),OpColon(2:42-2:43),UpperIdent(2:44-2:46),OpArrow(2:47-2:49),UpperIdent(2:50-2:52),CloseCurly(2:53-2:54),
KwExposes(3:2-3:9),OpenSquare(3:10-3:11),UpperIdent(3:11-3:13),Comma(3:13-3:14),UpperIdent(3:15-3:17),CloseSquare(3:17-3:18),
KwPackages(4:2-4:10),OpenCurly(4:11-4:12),LowerIdent(4:13-4:16),OpColon(4:16-4:17),StringStart(4:18-4:19),StringPart(4:19-4:22),StringEnd(4:22-4:23),Comma(4:23-4:24),LowerIdent(4:25-4:28),OpColon(4:28-4:29),StringStart(4:30-4:31),StringPart(4:31-4:34),StringEnd(4:34-4:35),CloseCurly(4:36-4:37),
KwProvides(6:2-6:10),OpenCurly(6:11-6:12),LowerIdent(6:13-6:16),OpColon(6:16-6:17),StringStart(6:18-6:19),StringPart(6:19-6:34),StringEnd(6:34-6:35),Comma(6:35-6:36),LowerIdent(6:37-6:40),OpColon(6:40-6:41),StringStart(6:42-6:43),StringPart(6:43-6:58),StringEnd(6:58-6:59),CloseCurly(6:60-6:61),
EndOfFile(7:1-7:1),
~~~
# PARSE
~~~clojure
(file @1.1-6.61
	(platform @1.1-6.61 (name "pf")
		(rigids @2.11-2.21
			(exposed-upper-ident @2.13-2.15 (text "R1"))
			(exposed-upper-ident @2.17-2.19 (text "R2")))
		(ty-record @2.22-2.54
			(anno-record-field @2.24-2.37 (name "r1")
				(ty-fn @2.29-2.37
					(ty @2.29-2.31 (name "R1"))
					(ty @2.35-2.37 (name "R2"))))
			(anno-record-field @2.39-2.52 (name "r2")
				(ty-fn @2.44-2.52
					(ty @2.44-2.46 (name "R1"))
					(ty @2.50-2.52 (name "R2")))))
		(exposes @3.10-3.18
			(exposed-upper-ident @3.11-3.13 (text "E1"))
			(exposed-upper-ident @3.15-3.17 (text "E2")))
		(packages @4.11-4.37
			(record-field @4.13-4.23 (name "pa1")
				(e-string @4.18-4.23
					(e-string-part @4.19-4.22 (raw "pa1"))))
			(record-field @4.25-4.35 (name "pa2")
				(e-string @4.30-4.35
					(e-string-part @4.31-4.34 (raw "pa2")))))
		(provides @6.11-6.61
			(record-field @6.13-6.35 (name "pr1")
				(e-string @6.18-6.35
					(e-string-part @6.19-6.34 (raw "not implemented"))))
			(record-field @6.37-6.59 (name "pr2")
				(e-string @6.42-6.59
					(e-string-part @6.43-6.58 (raw "not implemented"))))))
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
