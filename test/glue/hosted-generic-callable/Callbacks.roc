Callbacks := [].{
	install! : Box(({ value : a } => {})) => {}
	callback! : {} => Box(({} => { value : a }))
	install_record! : { init! : Box(({ value : a } => {})), render! : Box(({} => { value : a })) } => {}
}
