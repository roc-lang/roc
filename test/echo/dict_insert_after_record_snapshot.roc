# A dict that a fold's state record holds and that a snapshot step also
# appends to a list keeps its entries when a later step inserts into the
# state's dict (https://github.com/roc-lang/roc/issues/13031).
Alias : { dict : Dict(U16, U8), expected : List((U16, U8)) }
State : { dict : Dict(U16, U8), model : List((U16, U8)), aliases : List(Alias) }
Step : [Put(U16, U8), Snapshot]

model_put : List((U16, U8)), U16, U8 -> List((U16, U8))
model_put = |model, key, value| {
	without = List.drop_if(model, |(k, _)| k == key)
	if List.len(without) == List.len(model) {
		List.concat(model, [(key, value)])
	} else {
		List.map(model, |(k, v)| if k == key { (k, value) } else { (k, v) })
	}
}

apply : State, Step -> State
apply = |state, step|
	match step {
		Put(k, v) => { dict: Dict.insert(state.dict, k, v), model: model_put(state.model, k, v), aliases: state.aliases }
		Snapshot => {
			dict: state.dict,
			model: state.model,
			aliases: List.append(state.aliases, { dict: state.dict, expected: state.model }),
		}
	}

report! : List(Step) => {}
report! = |steps| {
	final = List.fold(steps, { dict: Dict.empty(), model: [], aliases: [] }, apply)
	for alias in final.aliases {
		echo!("${Str.inspect(Dict.to_list(alias.dict))} ${Str.inspect(alias.expected)}\n")
	}
	echo!("${Str.inspect(Dict.len(final.dict))} ${Str.inspect(List.len(final.model))}\n")
}

main! = |args| {
	key = 256 + U64.to_u16_wrap(List.len(args))
	report!([Put(key, 9), Snapshot, Put(key, 0)])
	report!([Put(key, 9), Snapshot, Put(1, 0)])
	Ok({})
}
