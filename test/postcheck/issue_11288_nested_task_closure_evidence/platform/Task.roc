Task(msg) := { run! : Box(() => msg) }.{
	spawn! : (() => msg) => Task(msg)
	spawn! = |thunk!| { run!: Box.box(thunk!) }

	await! : Task(msg) => msg
	await! = |task| {
		run! = Box.unbox(task.run!)
		run!()
	}
}
