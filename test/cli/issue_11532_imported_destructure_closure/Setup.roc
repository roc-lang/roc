# Repro for https://github.com/roc-lang/roc/issues/11532: this imported
# top-level destructure binds a value whose payload contains a closure.
module [cfg]

Cfg :: { label : Str, build : U32 -> Str }.{
	new : Str -> Try(Cfg, [Bad(Str)])
	new = |label| Ok(Cfg.({ label, build: |_n| label }))

	label : Cfg -> Str
	label = |Cfg.(config)| config.label
}

Ok(cfg) = Cfg.new("fine")
