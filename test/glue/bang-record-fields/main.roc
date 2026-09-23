platform "glue-bang-record-fields"
	requires {
		unused : {} -> {}
	}
	exposes []
	packages {}
	provides { "roc_callbacks": callbacks }
	targets: {}

callbacks : {} -> { init! : Box(({} => {})), render! : Box((U64 => Str)) }
callbacks = |_| { init!: Box.box(|{}| {}), render!: Box.box(|n| U64.to_str(n)) }
