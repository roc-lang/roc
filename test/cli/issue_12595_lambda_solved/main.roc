import Values

Payload : { call : (U64 -> U64), offset : U64 }

Message : [First(Payload), Second(Payload), Third(Payload)]

run : Message -> U64
run = |message| {
	payload = match message {
		First(value) => value
		Second(value) => value
		Third(value) => value
	}
	callback = payload.call
	callback(payload.offset)
}

Chain := [Done(U64), Next({} -> Chain)].{
	make : U64 -> Chain
	make = |n| if (n == 0) Done(n) else Next(|{}| make(n - 1))
	finish : Chain -> U64
	finish = |chain| match chain {
		Done(value) => value
		Next(next) => finish(next({}))
	}
}

count : U64, (U64 -> U64) -> U64
count = |n, callback| if (n == 0) callback(0) else count(n - 1, callback)

expect run(First({ call: Values.stored, offset: 3 })) == 10
expect run(Second({ call: Values.make(2), offset: 4 })) == 6
expect run(Third({ call: |n| n * 2, offset: 5 })) == 10
expect count(4, Values.stored) == 7 and Chain.finish(Chain.make(4)) == 0
expect {
	callback = Box.unbox(Box.box(Values.make(9)))
	callback(1) == 10
}
