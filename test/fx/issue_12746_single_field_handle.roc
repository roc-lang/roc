app [main!] { pf: platform "./platform/main.roc" }

import pf.Proc
import pf.Stdout

# A platform method whose argument's only refcounted content is a handle, passed
# to a hosted call that consumes it, and whose body rebuilds each result. A
# caller that switches on that result and keeps using the argument gets it back
# intact: the host must still find the handle live at the later `peek!`. The
# host crashes on a handle whose allocation is already freed.

method_call! : () => Try({}, [EndOfInput, ReadErr(Str)])
method_call! = || {
	reader = Proc.Reader.open!(1)
	_ = reader.read!()?
	n = reader.peek!()?
	Stdout.line!("method call: ${n.to_str()}")
	Ok({})
}

qualified_call! : () => Try({}, [EndOfInput, ReadErr(Str)])
qualified_call! = || {
	reader = Proc.Reader.open!(2)
	_ = Proc.Reader.read!(reader)?
	n = reader.peek!()?
	Stdout.line!("qualified call: ${n.to_str()}")
	Ok({})
}

match_return! : () => Try({}, [EndOfInput, ReadErr(Str), Failed])
match_return! = || {
	reader = Proc.Reader.open!(3)
	_ = match reader.read!() {
		Ok(n) => n
		Err(_) => {
			return Err(Failed)
		}
	}
	n = reader.peek!()?
	Stdout.line!("match return: ${n.to_str()}")
	Ok({})
}

read_times! : Proc.Reader, U64 => Try({}, [EndOfInput, ReadErr(Str)])
read_times! = |reader, remaining|
	if remaining == 0 {
		Ok({})
	} else {
		_ = reader.read!()?
		read_times!(reader, remaining - 1)
	}

recursive_helper! : () => Try({}, [EndOfInput, ReadErr(Str)])
recursive_helper! = || {
	reader = Proc.Reader.open!(4)
	read_times!(reader, 3)?
	n = reader.peek!()?
	Stdout.line!("recursive helper: ${n.to_str()}")
	Ok({})
}

scalar_sibling! : () => Try({}, [EndOfInput, ReadErr(Str)])
scalar_sibling! = || {
	counted = Proc.Counted.open!(5)
	_ = counted.read!()?
	n = counted.peek!()?
	Stdout.line!("scalar sibling: ${n.to_str()}")
	Ok({})
}

tag_payload! : () => Try({}, [EndOfInput, ReadErr(Str)])
tag_payload! = || {
	tagged = Proc.Tagged.open!(6)
	_ = tagged.read!()?
	n = tagged.peek!()?
	Stdout.line!("tag payload: ${n.to_str()}")
	Ok({})
}

main! = || {
	_ = method_call!()
	_ = qualified_call!()
	_ = match_return!()
	_ = recursive_helper!()
	_ = scalar_sibling!()
	_ = tag_payload!()
}
