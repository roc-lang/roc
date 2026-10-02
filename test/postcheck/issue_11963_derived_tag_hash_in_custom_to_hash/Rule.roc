import Sub

Rule :: { schedule : [Calendar(I64), Subdaily(Sub)] }.{
	Definition : { pattern : [Calendar(I64), Subdaily({ frequency : Sub.Frequency, interval : I64 })] }

	calendar : I64 -> Rule
	calendar = |n| { schedule: Calendar(n) }

	subdaily : Sub -> Rule
	subdaily = |sub| { schedule: Subdaily(sub) }

	definition : Rule -> Definition
	definition = |rule| {
		pattern = match rule.schedule {
			Calendar(value) => Calendar(value)
			Subdaily(value) => {
				data = Sub.definition(value)
				Subdaily({ frequency: data.frequency, interval: data.interval })
			}
		}
		{ pattern: pattern }
	}
}
