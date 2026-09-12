platform ""
	requires {
		[Msg : msg] for program : {
			decode : Str -> msg,
			update : List(msg) -> {},
		}
	}
	exposes [Host, Files, Io, Task]
	packages {}
	provides {
		"roc_main": main_for_host!,
		"roc_spawn_read": spawn_read_for_host!,
	}
	hosted {
		"roc_host_read_wait": Host.read_wait!,
	}
	targets: {
		inputs_dir: "targets/",
		x64musl: {
			inputs: [app],
			output: Archive,
		},
	}

import Host
import Files
import Io exposing [Io]
import Task exposing [Task]

# The spawned closure is a zero-argument nested procedure that captures `io`,
# selects `io.files()`, waits on the hosted read, and hands the contents to the
# app's message constructor. Its type mentions this export's quantified `Msg`.
spawn_read_for_host! : Io => Task(Msg)
spawn_read_for_host! = |io| Task.spawn!(|| (program.decode)(io.files().read!("small.txt")))

main_for_host! : {} => I32
main_for_host! = |_| {
	task = spawn_read_for_host!(Io.new({}))
	msg = Task.await!(task)
	(program.update)([msg])
	0
}
