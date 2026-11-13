platform "glue-opaque-ownership"
	requires {
		[Model : model] for program : {
			init : {} -> model,
		}
	}
	exposes [Types, Host]
	packages {}
	provides {
		"give_view": give_view,
		"give_handle": give_handle,
		"give_wrapped_handle": give_wrapped_handle,
		"give_slot": give_slot,
		"give_boxes": give_boxes,
		"give_items": give_items,
		"give_boxed_view": give_boxed_view,
		"give_labeled": give_labeled,
		"release_model": release_model,
	}
	hosted {
		"host_store": Host.store!,
		"host_keep": Host.keep!,
	}
	targets: {}

import Types
import Host

give_view : Box(Model) -> Types.View(Model)
give_view = |model| { title: "", model, tags: [] }

give_handle : Box(Model) -> Types.Handle(Model)
give_handle = |state| { state, count: 0 }

give_wrapped_handle : Box(Types.Wrap(Model)) -> { state : Box(Types.Wrap(Model)), count : U64 }
give_wrapped_handle = |state| { state, count: 0 }

give_slot : Box(Model) -> Types.Slot(Model)
give_slot = |model| Full(model)

give_boxes : List(Box(Model)) -> List(Types.Handle(Model))
give_boxes = |boxes| List.map(boxes, |state| { state, count: 0 })

give_items : List(Model) -> List(Types.Wrap(Model))
give_items = |items| List.map(items, |model| { model, n: 0 })

give_boxed_view : Box(Types.View(Model)) -> Box(Types.View(Model))
give_boxed_view = |view| view

give_labeled : List(Types.Labeled) -> Types.Labeled
give_labeled = |_| { label: "", count: 0 }

release_model : Box(Model) -> {}
release_model = |_| {}
