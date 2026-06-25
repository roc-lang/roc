app [main!] { pf: platform "./static-lib-platform/main.roc" }

TitleState : {
	frame_count : U64,
}

GameState : {
	frame_count : U64,
	last_generated : U64,
}

Model : [
	Title(TitleState),
	Game(GameState),
]

update_frame_count = |prev|
	{ ..prev, frame_count: prev.frame_count + 1 }

init_game = |state|
	Game({ frame_count: state.frame_count, last_generated: state.frame_count })

update_model = |model|
	match model {
		Title(prev) => init_game({ ..prev, frame_count: prev.frame_count + 1 })
		Game(prev) => Game(update_frame_count(prev))
	}

update_box : Box(Model) -> Box(Model)
update_box = |boxed| {
	model = Box.unbox(boxed)
	next = update_model(model)
	Box.box(next)
}

main! = |_seed| {
	first = update_box(Box.box(Title({ frame_count: 5 })))
	first_model = Box.unbox(first)
	model = update_model(first_model)

	match model {
		Game(game) if game.frame_count == 7 and game.last_generated == 6 => "ok"
		Game(game) if game.frame_count == 7 => "frame-ok"
		Game(game) if game.frame_count == 1000000000000000006 and game.last_generated == 6 => "bad-val"
		Game(game) if game.frame_count - game.last_generated == 1 => "diff-ok"
		Game(game) if game.frame_count - game.last_generated == 1000000000000000000 => "diff-1e18"
		Game(game) if game.frame_count - game.last_generated > 90 => "diff-high"
		Game(game) if game.frame_count == 6 and game.last_generated == 6 => "stale"
		Game(game) if game.last_generated == 6 => "last-ok"
		Game(_) => "wrong-game"
		_ => "not-game"
	}
}
