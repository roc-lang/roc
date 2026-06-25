import Node
import Signal

## Browser HTTP text capability.
##
## The Signals browser runtime routes task names with this prefix to `fetch`.
## Runtime policy stays explicit: today it accepts only the documented Signals
## dev-server ops endpoints and returns UTF-8 response text.
Http := [].{
	get_text_task = |purpose| {
		name = Str.concat("http:get-text:", purpose)
		Signal.task_source(name, |value| value, |err| err, False)
	}

	get_text = |task, path| Signal.start_str(task, path)
}
