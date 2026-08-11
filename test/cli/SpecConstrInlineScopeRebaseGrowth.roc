app [main!] { pf: platform "../fx-open/platform/main.roc" }

# A self-recursive effectful accumulator loop that matches on a nominal's
# effectful method and destructures its record accumulator in the parameter
# list. Case-of-case distribution re-clones this body once per step, and each
# re-clone used to re-base the whole inline scope chain onto a scope that
# already carried it, so the chain doubled every step until the compiler ran
# out of stack walking it. Raising `case_of_case_work_budget` to 65536 made the
# cascade long enough to reach that point on a twenty-line program.

import pf.Fallible
import pf.Stdout

Reader := { label : Str }.{
	read_line! : Reader => Try(List(U8), [HostErr(Str), ..])
	read_line! = |reader| {
		raw = Fallible.via_question!({})?
		Ok(decorate(reader, raw).to_utf8())
	}

	decorate : Reader, Str -> Str
	decorate = |reader, raw| "${reader.label}${raw}"
}

main! : List(Str) => Try({}, [Exit(I32), HostErr(Str), ..])
main! = |_args| {
	reader = Reader.{ label: "x" }
	summary = read_all!(reader, { lines_read: 0, bytes_read: 0 })?
	Stdout.line!("Done: ${Str.inspect(summary)}")
	Ok({})
}

read_all! : Reader, { lines_read : U64, bytes_read : U64 } => Try({ lines_read : U64, bytes_read : U64 }, _)
read_all! = |reader, { lines_read, bytes_read }|
	match reader.read_line!() {
		Ok(bytes) if bytes.len() == 0 =>
			Ok({ lines_read, bytes_read })

		Ok(bytes) =>
			read_all!(
				reader,
				{
					lines_read: lines_read + 1,
					bytes_read: bytes_read + bytes.len(),
				},
			)

		Err(err) => Err(err)
	}
