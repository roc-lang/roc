Model := [].{
	Handle := {}.{
		read : Handle, U64 -> Try({}, [ReadErr([Denied, Gone]), WriteErr([Denied, Gone])])
		read = |_, n| if n == 0 Err(ReadErr(Denied)) else Err(WriteErr(Gone))
	}
	# The stored closure retains read evidence from the recursive callback.
	make = |handle| |n| wait_next(n, handle)
	wait_next = |n, handle| execute({
		run: || handle.read(n),
		resolve: |result| match result {
			Ok({}) => "ok"
			Err(error) => if n > 1 wait_next(n - 1, handle) else describe(error)
		},
	})
}

# Wildcards leave the payload rows open until read supplies Denied and Gone.
describe = |error| match error {
	ReadErr(Denied) => "read denied"
	ReadErr(_) => "read error"
	WriteErr(Denied) => "write denied"
	WriteErr(_) => "write error"
}

execute : { run : (() -> result), resolve : (result -> Str) } -> Str
execute = |task| {
	run = task.run
	resolve = task.resolve
	resolve(run())
}
