app [process_string] { pf: platform "./platform/main.roc" }

import Message

process_string : Str -> Str
process_string = |input| {
	msg = Message.msg()
	"${msg}${input}"
}
