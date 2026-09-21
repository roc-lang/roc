platform "glue-provided-value-shared-type"
	requires { main! : () => {} }
	exposes []
	packages {}
	provides { "roc_default_shared": default_shared, "roc_echo": echo }
	targets: {}

Shared : { count : U64, label : Str }

default_shared : Shared
default_shared = { count: 0, label: "default" }

echo : Shared -> Shared
echo = |value| value
