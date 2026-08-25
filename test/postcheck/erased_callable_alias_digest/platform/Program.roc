Program := [].{
	FileReadError : [
		NotFound,
		ReadFailed,
		Busy,
		Unavailable,
		TooLarge,
	]

	SmallFileError : [
		NotFound,
		ReadFailed,
		Busy,
		Unavailable,
		TooLarge,
		NotUtf8,
	]

	CompletionFromHost : {
		kind : U8,
		contents : Str,
		bytes : List(U8),
	}

	CompletionEnvelope(msg) : {
		raw : CompletionFromHost,
		deliver : Box(CompletionFromHost -> Box(msg)),
	}

	complete : CompletionEnvelope(msg) -> msg
	complete = |completion| {
		deliver = Box.unbox(completion.deliver)
		Box.unbox(deliver(completion.raw))
	}
}
