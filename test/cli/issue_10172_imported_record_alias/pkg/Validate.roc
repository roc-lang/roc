import Base exposing [OptionConfig]

Validate := [].{
	OptionAtPath : { option : OptionConfig }
	ValidationErr : [InvalidOption(OptionAtPath)]

	validate : List(OptionConfig) -> Try({}, ValidationErr)
	validate = |options|
		Validate.check_options(options.map(|option| { option: option }))

	check_options : List(OptionAtPath) -> Try({}, ValidationErr)
	check_options = |_options| Ok({})
}
