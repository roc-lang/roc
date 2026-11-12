platform "glue-boxed-app-type"
	requires {
		[Model : model] for program : {
			init : {} -> model,
		}
	}
	exposes [Types]
	packages {}
	provides {
		"take_nominal": take_nominal,
		"take_record": take_record,
		"take_tuple": take_tuple,
		"take_tag_union": take_tag_union,
		"take_nested": take_nested,
		"take_recursive": take_recursive,
		"take_list": take_list,
		"take_bare_list": take_bare_list,
		"take_holder": take_holder,
		"take_indirect": take_indirect,
		"take_known": take_known,
		"take_bare": take_bare,
	}
	targets: {}

import Types

take_nominal : Box(Types.Wrap(Model)) -> U64
take_nominal = |_| 0

take_record : Box({ model : Model, n : U64 }) -> U64
take_record = |_| 0

take_tuple : Box((U64, Model)) -> U64
take_tuple = |_| 0

take_tag_union : Box([Empty, Loaded(Model)]) -> U64
take_tag_union = |_| 0

take_nested : Box(Types.Deep(Model)) -> U64
take_nested = |_| 0

take_recursive : Box(Types.Tree(Model)) -> U64
take_recursive = |_| 0

take_list : List(Types.Wrap(Model)) -> U64
take_list = |_| 0

take_bare_list : List(Model) -> U64
take_bare_list = |_| 0

take_holder : { state : Box(Types.Wrap(Model)), count : U64 } -> U64
take_holder = |_| 0

take_indirect : Box(Types.Indirect(Model)) -> U64
take_indirect = |_| 0

take_known : Box(Types.Known) -> U64
take_known = |_| 0

take_bare : Box(Model) -> U64
take_bare = |_| 0
