app [main!] { pf: platform "./platform/main.roc" }

import pf.Proc
import pf.Stdout

# Issue 11286: `Proc.poll!` rebuilds the error of the hosted
# `Proc.handle_poll!`, so its row is its own and it serves a `where`
# requirement, whose `Try` error row lists exactly its tags. The requiring
# functions compose that row into their own wider one with `?`.

Broken := [Broken].{
	poll! : Broken => Try(U64, [PollErr(Str), Closed])
	poll! = |_| Err(Closed)
}

poll_twice! : a => Try(U64, [PollErr(Str), Closed, Changed(U64, U64)]) where [a.poll! : a => Try(U64, [PollErr(Str), Closed])]
poll_twice! = |x| {
	first = x.poll!()?
	second = x.poll!()?
	if first == second Ok(first + second) else Err(Changed(first, second))
}

forward! : a => Try(U64, [PollErr(Str), Closed, Changed(U64, U64)]) where [a.poll! : a => Try(U64, [PollErr(Str), Closed])]
forward! = |x| Ok(x.poll!()?)

show : Try(U64, [PollErr(Str), Closed, Changed(U64, U64)]) -> Str
show = |result|
	match result {
		Ok(n) => "ok ${n.to_str()}"
		Err(PollErr(message)) => "poll error ${message}"
		Err(Closed) => "closed"
		Err(Changed(first, second)) => "changed ${first.to_str()} ${second.to_str()}"
	}

main! = || {
	match Proc.spawn!("where") {
		Ok(child) => {
			Stdout.line!("twice: ${show(poll_twice!(child))}")
			Stdout.line!("forward: ${show(forward!(child))}")
			_ = child.close!()
		}
		Err(SpawnErr(message)) => Stdout.line!("spawn error ${message}")
	}
	Stdout.line!("broken twice: ${show(poll_twice!(Broken.Broken))}")
	Stdout.line!("broken forward: ${show(forward!(Broken.Broken))}")
}
