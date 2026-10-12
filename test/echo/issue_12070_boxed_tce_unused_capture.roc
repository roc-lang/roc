# repro for https://github.com/roc-lang/roc/issues/12070
# `step!` is reached through a boxed closure, so it carries an erased capture
# argument it never reads, and its self-call on `Skip` makes it a tail-call
# loop. Every backend must build it and print the length of the first
# non-empty chunk.

Producer := [Producer(Box({} => Step))]
Step := [Chunk({ bytes : List(U8), next : Producer }), End]

from_stream : Stream(List(U8)) -> Producer
from_stream = |stream| Producer.Producer(Box.box(|{}| step!(stream.drop_if(|bytes| bytes.is_empty()))))

step! : Stream(List(U8)) => Step
step! = |stream|
	match stream.next!() {
		One({ item, .. }) => Step.Chunk({ bytes: item, next: Producer.Producer(Box.box(|{}| Step.End)) })
		Skip({ rest }) => step!(rest)
		Done => Step.End
	}

main! = |_| {
	stream = Stream.custom(0, Known(2), |n| if n > 1 { Err(NoMore) } else { Ok((if n == 0 { [] } else { "abc".to_utf8() }, n + 1)) })
	Producer.Producer(boxed) = from_stream(stream)
	run! = Box.unbox(boxed)
	length = match run!({}) {
		Step.Chunk({ bytes, .. }) => bytes.len()
		Step.End => 0
	}
	echo!(length.to_str())
	Ok({})
}
