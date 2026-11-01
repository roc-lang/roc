app [main!] { pf: platform "./platform/main.roc" }

import pf.Proc
import pf.Stdout

# A platform method shaped like basic-cli's `Tcp.Connection.read_exactly!`: its
# record argument holds a handle beside scalar settings, and it reads the handle
# and then calls a method on a sibling field while evaluating the arguments of a
# hosted call that consumes the handle. A caller that switches on the result and
# keeps using the record gets the record back intact: the host must still find
# the handle live at the later `peek!`. The host crashes on a handle whose
# allocation is already freed.

run! : () => Try({}, [UnexpectedEndOfInput, ReadErr(Str)])
run! = || {
	conn = Proc.Connection.open!(1)
	read = conn.read_exactly!(5)?
	n = conn.peek!()?
	Stdout.line!("connection: ${read.to_str()} ${n.to_str()}")
	Ok({})
}

main! = || {
	_ = run!()
}
