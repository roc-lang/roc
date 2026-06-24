# META
~~~ini
description=platform header targets section with per-target inputs and output kinds
type=file
~~~
# SOURCE
~~~roc
platform "foo"
    requires {}
    exposes []
    packages {}
    provides {}
    targets: {
        inputs_dir: "targets/",
        x64glibc: { inputs: [app] },
        wasm32: { inputs: ["libhost.a", app], output: Shared },
        arm64mac: { inputs: ["libhost.a", app], output: Shared },
        x64musl: { inputs: [app], output: Archive },
    }
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
KwPlatform,StringStart,StringPart,StringEnd,
KwRequires,OpenCurly,CloseCurly,
KwExposes,OpenSquare,CloseSquare,
KwPackages,OpenCurly,CloseCurly,
KwProvides,OpenCurly,CloseCurly,
KwTargets,OpColon,OpenCurly,
LowerIdent,OpColon,StringStart,StringPart,StringEnd,Comma,
LowerIdent,OpColon,OpenCurly,LowerIdent,OpColon,OpenSquare,KwApp,CloseSquare,CloseCurly,Comma,
LowerIdent,OpColon,OpenCurly,LowerIdent,OpColon,OpenSquare,StringStart,StringPart,StringEnd,Comma,KwApp,CloseSquare,Comma,LowerIdent,OpColon,UpperIdent,CloseCurly,Comma,
LowerIdent,OpColon,OpenCurly,LowerIdent,OpColon,OpenSquare,StringStart,StringPart,StringEnd,Comma,KwApp,CloseSquare,Comma,LowerIdent,OpColon,UpperIdent,CloseCurly,Comma,
LowerIdent,OpColon,OpenCurly,LowerIdent,OpColon,OpenSquare,KwApp,CloseSquare,Comma,LowerIdent,OpColon,UpperIdent,CloseCurly,Comma,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(platform (name "foo")
		(requires)
		(exposes)
		(packages)
		(provides))
	(statements))
~~~
# FORMATTED
~~~roc
platform "foo"
	requires {}
	exposes []
	packages {}
	provides {}
	targets: {
		inputs_dir: "targets/",
		x64glibc: { inputs: [app] },
		wasm32: {
			inputs: ["libhost.a", app],
			output: Shared,
		},
		arm64mac: {
			inputs: ["libhost.a", app],
			output: Shared,
		},
		x64musl: {
			inputs: [app],
			output: Archive,
		},
	}
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
