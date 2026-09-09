platform "glue-tag-union-layouts"
	requires {
		main! : () => {}
	}
	exposes [Shapes]
	packages {}
	provides { "roc_main": main_for_host! }
	hosted {
		"roc_first_scope": Shapes.first_scope!,
		"roc_second_scope": Shapes.second_scope!,
		"roc_nested": Shapes.nested!,
		"roc_tuple": Shapes.tuple!,
		"roc_mixed": Shapes.mixed!,
		"roc_scalar": Shapes.scalar!,
		"roc_nested_payload": Shapes.nestedPayload!,
		"roc_mixed_empty_payload": Shapes.mixedEmptyPayload!,
	}
	targets: {}

import Shapes

main_for_host! : () => {}
main_for_host! = main!
