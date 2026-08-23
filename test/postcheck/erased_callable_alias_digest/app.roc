app [Msg, program] { pf: platform "./platform/main.roc" }

# A boxed callback is first erased while Program.complete is solved, then
# revisited through the provided host boundary. The two views of Msg can differ
# only in whether these transparent aliases have already been expanded. They
# must still produce the same erased-callable source type digest.
import pf.Program

Msg : [
	SmallReadFinished(Try(Str, Program.SmallFileError)),
	BytesReadFinished(Try(List(U8), Program.FileReadError)),
]

program = {
	update,
}

update : List(Msg) -> {}
update = |_messages| {}
