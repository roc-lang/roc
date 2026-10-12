Sub :: { frequency : Frequency, interval : I64 }.{
	Frequency : [Hourly, Minutely, Secondly]

	new : Frequency, I64 -> Sub
	new = |frequency, interval| { frequency, interval }

	definition : Sub -> { frequency : Frequency, interval : I64 }
	definition = |value| { frequency: value.frequency, interval: value.interval }
}
