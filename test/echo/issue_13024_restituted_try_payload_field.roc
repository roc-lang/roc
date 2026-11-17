# A field moved out of an `Ok` payload into a call whose failure outcome hands
# it back, then moved out again after the `??` join. The payload's fields are
# numbered in a view of the `Try`, so the returned field refills the `Try`'s
# own unit.

poll : Str -> Try(Str, [Closed])
poll = |child| if child == "" Ok("r") else Err(Closed)

main! = |args| {
	tasks = List.map(args, |a| { child: a, buf: "" })
	match List.first(tasks) {
		Ok(task) => {
			_ = poll(task.child) ?? "d"
			_ = List.append(tasks, { child: task.child, buf: "y" })
		}
		Err(_) => {}
	}
	echo!("done\n")
	Ok({})
}
