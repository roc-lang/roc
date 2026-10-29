app [main!] { pf: platform "./platform/main.roc" }

import pf.Proc
import pf.Stdout

# A platform method that passes a field of its record argument to a hosted
# call and matches on the result. A caller that switches on that result with
# `?` and keeps using the record gets the record back intact: the host must
# still find the handle live at the later `close!`. The host crashes on a
# handle whose allocation is already freed.

method_call! : () => Try({}, [SpawnErr(Str), PollErr(Str), Closed])
method_call! = || {
	child = Proc.spawn!("method")?
	_ = child.poll!()?
	_ = child.close!()
	Stdout.line!("method call: closed")
	Ok({})
}

qualified_call! : () => Try({}, [SpawnErr(Str), PollErr(Str), Closed])
qualified_call! = || {
	child = Proc.spawn!("qualified")?
	_ = Proc.poll!(child)?
	_ = child.close!()
	Stdout.line!("qualified call: closed")
	Ok({})
}

poll_times! : Proc, U64 => Try({}, [PollErr(Str), Closed])
poll_times! = |child, remaining|
	if remaining == 0 {
		Ok({})
	} else {
		_ = child.poll!()?
		poll_times!(child, remaining - 1)
	}

recursive_helper! : () => Try({}, [SpawnErr(Str), PollErr(Str), Closed])
recursive_helper! = || {
	child = Proc.spawn!("recursive")?
	poll_times!(child, 3)?
	_ = child.close!()
	Stdout.line!("recursive helper: closed")
	Ok({})
}

wrapped_poll! : Proc => Try(U64, [PollErr(Str), Closed])
wrapped_poll! = |child| child.poll!()

wrapper_call! : () => Try({}, [SpawnErr(Str), PollErr(Str), Closed])
wrapper_call! = || {
	child = Proc.spawn!("wrapper")?
	_ = wrapped_poll!(child)?
	_ = child.close!()
	Stdout.line!("wrapper call: closed")
	Ok({})
}

report! : Try({}, [SpawnErr(Str), PollErr(Str), Closed]) => {}
report! = |result|
	match result {
		Ok({}) => {}
		Err(_) => Stdout.line!("error")
	}

main! = || {
	report!(method_call!())
	report!(qualified_call!())
	report!(recursive_helper!())
	report!(wrapper_call!())
}
