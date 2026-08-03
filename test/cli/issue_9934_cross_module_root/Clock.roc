# Companion module for Main.roc: `minutes_per_day` is a compile-time-evaluable
# local constant used twice in one expression, so checking hoists it into this
# module's compile-time root table where it gets root id 0.
Clock :: { hour : U8, minute : U8 }.{
	create : { hour : I64, minute : I64 } -> Clock
	create = |{ hour, minute }| {
		hour24 = (hour % 24 + minute // 60) % 24
		minute60 = minute % 60
		minutes_per_day = 24 * 60
		total_minute = (hour24 * 60 + minute60 + minutes_per_day) % minutes_per_day
		hh = (total_minute // 60).to_u8_wrap()
		mm = (total_minute % 60).to_u8_wrap()
		{ hour: hh, minute: mm }
	}
}
