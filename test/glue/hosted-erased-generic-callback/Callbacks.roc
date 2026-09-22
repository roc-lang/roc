Callbacks := [].{
	Event :: { value : U64 }

	install! : Box(({ value : a } => {})) => {}
	notify! : Box((Event => {})) => {}
}
