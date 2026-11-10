## Boundary values at every integer width survive shared-list and slice updates.
## The callback produces allocated strings, so its result type remains generic
## and owns references independently of the integer item binding.
BoxyListIntOwnership := [].{}

second : List(a) -> Try(a, [OutOfBounds])
second = |items| items.get(1)

# Persist the nested closures of the pre-specialized List.iter worker.
saved_u8 : Iter(U8)
saved_u8 = [U8.lowest, U8.highest].iter()

check_u8 : U8, U8, Str -> Bool
check_u8 = |low, high, prefix| {
	items = List.with_capacity(4).append(low).append(high).append(low)
	slice = items.sublist({ start: 1, len: 2 })
	changed = items.set(0, high)
	grown = items.append(high)
	changed_slice = slice.set(1, high)
	grown_slice = slice.append(high)
	names = items.map(|item| prefix.concat(Str.inspect(item)))
	items == [low, high, low]
		and slice == [high, low]
		and changed == Ok([high, high, low])
		and grown == [low, high, low, high]
		and changed_slice == Ok([high, high])
		and grown_slice == [high, low, high]
		and items.get(0) == Ok(low)
		and items.get(1) == Ok(high)
		and items.get(3) == Err(OutOfBounds)
		and items.set(3, high) == Err(OutOfBounds)
		and items.first() == Ok(low)
		and items.last() == Ok(low)
		and items.contains(high)
		and items.concat(slice) == [low, high, low, high, low]
		and second(items) == Ok(high)
		and items.keep_if(|item| item == high) == [high]
		and grown.map(|item| item) == [low, high, low, high]
		and items.iter_rev().fold([], |acc, item| acc.append(item)) == [low, high, low]
		and saved_u8.fold([], |acc, item| acc.append(item)) == [U8.lowest, U8.highest]
		and items.fold(0, |count, _item| count + 1) == 3
		and names == [prefix.concat(Str.inspect(low)), prefix.concat(Str.inspect(high)), prefix.concat(Str.inspect(low))]
}

expect check_u8(U8.lowest, U8.highest, "a captured prefix longer than an inline string: ")

# Persist the nested closures of the pre-specialized List.iter worker.
saved_i8 : Iter(I8)
saved_i8 = [I8.lowest, I8.highest].iter()

check_i8 : I8, I8, Str -> Bool
check_i8 = |low, high, prefix| {
	items = List.with_capacity(4).append(low).append(high).append(low)
	slice = items.sublist({ start: 1, len: 2 })
	changed = items.set(0, high)
	grown = items.append(high)
	changed_slice = slice.set(1, high)
	grown_slice = slice.append(high)
	names = items.map(|item| prefix.concat(Str.inspect(item)))
	items == [low, high, low]
		and slice == [high, low]
		and changed == Ok([high, high, low])
		and grown == [low, high, low, high]
		and changed_slice == Ok([high, high])
		and grown_slice == [high, low, high]
		and items.get(0) == Ok(low)
		and items.get(1) == Ok(high)
		and items.get(3) == Err(OutOfBounds)
		and items.set(3, high) == Err(OutOfBounds)
		and items.first() == Ok(low)
		and items.last() == Ok(low)
		and items.contains(high)
		and items.concat(slice) == [low, high, low, high, low]
		and second(items) == Ok(high)
		and items.keep_if(|item| item == high) == [high]
		and grown.map(|item| item) == [low, high, low, high]
		and items.iter_rev().fold([], |acc, item| acc.append(item)) == [low, high, low]
		and saved_i8.fold([], |acc, item| acc.append(item)) == [I8.lowest, I8.highest]
		and items.fold(0, |count, _item| count + 1) == 3
		and names == [prefix.concat(Str.inspect(low)), prefix.concat(Str.inspect(high)), prefix.concat(Str.inspect(low))]
}

expect check_i8(I8.lowest, I8.highest, "a captured prefix longer than an inline string: ")

# Persist the nested closures of the pre-specialized List.iter worker.
saved_u16 : Iter(U16)
saved_u16 = [U16.lowest, U16.highest].iter()

check_u16 : U16, U16, Str -> Bool
check_u16 = |low, high, prefix| {
	items = List.with_capacity(4).append(low).append(high).append(low)
	slice = items.sublist({ start: 1, len: 2 })
	changed = items.set(0, high)
	grown = items.append(high)
	changed_slice = slice.set(1, high)
	grown_slice = slice.append(high)
	names = items.map(|item| prefix.concat(Str.inspect(item)))
	items == [low, high, low]
		and slice == [high, low]
		and changed == Ok([high, high, low])
		and grown == [low, high, low, high]
		and changed_slice == Ok([high, high])
		and grown_slice == [high, low, high]
		and items.get(0) == Ok(low)
		and items.get(1) == Ok(high)
		and items.get(3) == Err(OutOfBounds)
		and items.set(3, high) == Err(OutOfBounds)
		and items.first() == Ok(low)
		and items.last() == Ok(low)
		and items.contains(high)
		and items.concat(slice) == [low, high, low, high, low]
		and second(items) == Ok(high)
		and items.keep_if(|item| item == high) == [high]
		and grown.map(|item| item) == [low, high, low, high]
		and items.iter_rev().fold([], |acc, item| acc.append(item)) == [low, high, low]
		and saved_u16.fold([], |acc, item| acc.append(item)) == [U16.lowest, U16.highest]
		and items.fold(0, |count, _item| count + 1) == 3
		and names == [prefix.concat(Str.inspect(low)), prefix.concat(Str.inspect(high)), prefix.concat(Str.inspect(low))]
}

expect check_u16(U16.lowest, U16.highest, "a captured prefix longer than an inline string: ")

# Persist the nested closures of the pre-specialized List.iter worker.
saved_i16 : Iter(I16)
saved_i16 = [I16.lowest, I16.highest].iter()

check_i16 : I16, I16, Str -> Bool
check_i16 = |low, high, prefix| {
	items = List.with_capacity(4).append(low).append(high).append(low)
	slice = items.sublist({ start: 1, len: 2 })
	changed = items.set(0, high)
	grown = items.append(high)
	changed_slice = slice.set(1, high)
	grown_slice = slice.append(high)
	names = items.map(|item| prefix.concat(Str.inspect(item)))
	items == [low, high, low]
		and slice == [high, low]
		and changed == Ok([high, high, low])
		and grown == [low, high, low, high]
		and changed_slice == Ok([high, high])
		and grown_slice == [high, low, high]
		and items.get(0) == Ok(low)
		and items.get(1) == Ok(high)
		and items.get(3) == Err(OutOfBounds)
		and items.set(3, high) == Err(OutOfBounds)
		and items.first() == Ok(low)
		and items.last() == Ok(low)
		and items.contains(high)
		and items.concat(slice) == [low, high, low, high, low]
		and second(items) == Ok(high)
		and items.keep_if(|item| item == high) == [high]
		and grown.map(|item| item) == [low, high, low, high]
		and items.iter_rev().fold([], |acc, item| acc.append(item)) == [low, high, low]
		and saved_i16.fold([], |acc, item| acc.append(item)) == [I16.lowest, I16.highest]
		and items.fold(0, |count, _item| count + 1) == 3
		and names == [prefix.concat(Str.inspect(low)), prefix.concat(Str.inspect(high)), prefix.concat(Str.inspect(low))]
}

expect check_i16(I16.lowest, I16.highest, "a captured prefix longer than an inline string: ")

# Persist the nested closures of the pre-specialized List.iter worker.
saved_u32 : Iter(U32)
saved_u32 = [U32.lowest, U32.highest].iter()

check_u32 : U32, U32, Str -> Bool
check_u32 = |low, high, prefix| {
	items = List.with_capacity(4).append(low).append(high).append(low)
	slice = items.sublist({ start: 1, len: 2 })
	changed = items.set(0, high)
	grown = items.append(high)
	changed_slice = slice.set(1, high)
	grown_slice = slice.append(high)
	names = items.map(|item| prefix.concat(Str.inspect(item)))
	items == [low, high, low]
		and slice == [high, low]
		and changed == Ok([high, high, low])
		and grown == [low, high, low, high]
		and changed_slice == Ok([high, high])
		and grown_slice == [high, low, high]
		and items.get(0) == Ok(low)
		and items.get(1) == Ok(high)
		and items.get(3) == Err(OutOfBounds)
		and items.set(3, high) == Err(OutOfBounds)
		and items.first() == Ok(low)
		and items.last() == Ok(low)
		and items.contains(high)
		and items.concat(slice) == [low, high, low, high, low]
		and second(items) == Ok(high)
		and items.keep_if(|item| item == high) == [high]
		and grown.map(|item| item) == [low, high, low, high]
		and items.iter_rev().fold([], |acc, item| acc.append(item)) == [low, high, low]
		and saved_u32.fold([], |acc, item| acc.append(item)) == [U32.lowest, U32.highest]
		and items.fold(0, |count, _item| count + 1) == 3
		and names == [prefix.concat(Str.inspect(low)), prefix.concat(Str.inspect(high)), prefix.concat(Str.inspect(low))]
}

expect check_u32(U32.lowest, U32.highest, "a captured prefix longer than an inline string: ")

# Persist the nested closures of the pre-specialized List.iter worker.
saved_i32 : Iter(I32)
saved_i32 = [I32.lowest, I32.highest].iter()

check_i32 : I32, I32, Str -> Bool
check_i32 = |low, high, prefix| {
	items = List.with_capacity(4).append(low).append(high).append(low)
	slice = items.sublist({ start: 1, len: 2 })
	changed = items.set(0, high)
	grown = items.append(high)
	changed_slice = slice.set(1, high)
	grown_slice = slice.append(high)
	names = items.map(|item| prefix.concat(Str.inspect(item)))
	items == [low, high, low]
		and slice == [high, low]
		and changed == Ok([high, high, low])
		and grown == [low, high, low, high]
		and changed_slice == Ok([high, high])
		and grown_slice == [high, low, high]
		and items.get(0) == Ok(low)
		and items.get(1) == Ok(high)
		and items.get(3) == Err(OutOfBounds)
		and items.set(3, high) == Err(OutOfBounds)
		and items.first() == Ok(low)
		and items.last() == Ok(low)
		and items.contains(high)
		and items.concat(slice) == [low, high, low, high, low]
		and second(items) == Ok(high)
		and items.keep_if(|item| item == high) == [high]
		and grown.map(|item| item) == [low, high, low, high]
		and items.iter_rev().fold([], |acc, item| acc.append(item)) == [low, high, low]
		and saved_i32.fold([], |acc, item| acc.append(item)) == [I32.lowest, I32.highest]
		and items.fold(0, |count, _item| count + 1) == 3
		and names == [prefix.concat(Str.inspect(low)), prefix.concat(Str.inspect(high)), prefix.concat(Str.inspect(low))]
}

expect check_i32(I32.lowest, I32.highest, "a captured prefix longer than an inline string: ")

# Persist the nested closures of the pre-specialized List.iter worker.
saved_u64 : Iter(U64)
saved_u64 = [U64.lowest, U64.highest].iter()

check_u64 : U64, U64, Str -> Bool
check_u64 = |low, high, prefix| {
	items = List.with_capacity(4).append(low).append(high).append(low)
	slice = items.sublist({ start: 1, len: 2 })
	changed = items.set(0, high)
	grown = items.append(high)
	changed_slice = slice.set(1, high)
	grown_slice = slice.append(high)
	names = items.map(|item| prefix.concat(Str.inspect(item)))
	items == [low, high, low]
		and slice == [high, low]
		and changed == Ok([high, high, low])
		and grown == [low, high, low, high]
		and changed_slice == Ok([high, high])
		and grown_slice == [high, low, high]
		and items.get(0) == Ok(low)
		and items.get(1) == Ok(high)
		and items.get(3) == Err(OutOfBounds)
		and items.set(3, high) == Err(OutOfBounds)
		and items.first() == Ok(low)
		and items.last() == Ok(low)
		and items.contains(high)
		and items.concat(slice) == [low, high, low, high, low]
		and second(items) == Ok(high)
		and items.keep_if(|item| item == high) == [high]
		and grown.map(|item| item) == [low, high, low, high]
		and items.iter_rev().fold([], |acc, item| acc.append(item)) == [low, high, low]
		and saved_u64.fold([], |acc, item| acc.append(item)) == [U64.lowest, U64.highest]
		and items.fold(0, |count, _item| count + 1) == 3
		and names == [prefix.concat(Str.inspect(low)), prefix.concat(Str.inspect(high)), prefix.concat(Str.inspect(low))]
}

expect check_u64(U64.lowest, U64.highest, "a captured prefix longer than an inline string: ")

# Persist the nested closures of the pre-specialized List.iter worker.
saved_i64 : Iter(I64)
saved_i64 = [I64.lowest, I64.highest].iter()

check_i64 : I64, I64, Str -> Bool
check_i64 = |low, high, prefix| {
	items = List.with_capacity(4).append(low).append(high).append(low)
	slice = items.sublist({ start: 1, len: 2 })
	changed = items.set(0, high)
	grown = items.append(high)
	changed_slice = slice.set(1, high)
	grown_slice = slice.append(high)
	names = items.map(|item| prefix.concat(Str.inspect(item)))
	items == [low, high, low]
		and slice == [high, low]
		and changed == Ok([high, high, low])
		and grown == [low, high, low, high]
		and changed_slice == Ok([high, high])
		and grown_slice == [high, low, high]
		and items.get(0) == Ok(low)
		and items.get(1) == Ok(high)
		and items.get(3) == Err(OutOfBounds)
		and items.set(3, high) == Err(OutOfBounds)
		and items.first() == Ok(low)
		and items.last() == Ok(low)
		and items.contains(high)
		and items.concat(slice) == [low, high, low, high, low]
		and second(items) == Ok(high)
		and items.keep_if(|item| item == high) == [high]
		and grown.map(|item| item) == [low, high, low, high]
		and items.iter_rev().fold([], |acc, item| acc.append(item)) == [low, high, low]
		and saved_i64.fold([], |acc, item| acc.append(item)) == [I64.lowest, I64.highest]
		and items.fold(0, |count, _item| count + 1) == 3
		and names == [prefix.concat(Str.inspect(low)), prefix.concat(Str.inspect(high)), prefix.concat(Str.inspect(low))]
}

expect check_i64(I64.lowest, I64.highest, "a captured prefix longer than an inline string: ")

# Persist the nested closures of the pre-specialized List.iter worker.
saved_u128 : Iter(U128)
saved_u128 = [U128.lowest, U128.highest].iter()

check_u128 : U128, U128, Str -> Bool
check_u128 = |low, high, prefix| {
	items = List.with_capacity(4).append(low).append(high).append(low)
	slice = items.sublist({ start: 1, len: 2 })
	changed = items.set(0, high)
	grown = items.append(high)
	changed_slice = slice.set(1, high)
	grown_slice = slice.append(high)
	names = items.map(|item| prefix.concat(Str.inspect(item)))
	items == [low, high, low]
		and slice == [high, low]
		and changed == Ok([high, high, low])
		and grown == [low, high, low, high]
		and changed_slice == Ok([high, high])
		and grown_slice == [high, low, high]
		and items.get(0) == Ok(low)
		and items.get(1) == Ok(high)
		and items.get(3) == Err(OutOfBounds)
		and items.set(3, high) == Err(OutOfBounds)
		and items.first() == Ok(low)
		and items.last() == Ok(low)
		and items.contains(high)
		and items.concat(slice) == [low, high, low, high, low]
		and second(items) == Ok(high)
		and items.keep_if(|item| item == high) == [high]
		and grown.map(|item| item) == [low, high, low, high]
		and items.iter_rev().fold([], |acc, item| acc.append(item)) == [low, high, low]
		and saved_u128.fold([], |acc, item| acc.append(item)) == [U128.lowest, U128.highest]
		and items.fold(0, |count, _item| count + 1) == 3
		and names == [prefix.concat(Str.inspect(low)), prefix.concat(Str.inspect(high)), prefix.concat(Str.inspect(low))]
}

expect check_u128(U128.lowest, U128.highest, "a captured prefix longer than an inline string: ")

# Persist the nested closures of the pre-specialized List.iter worker.
saved_i128 : Iter(I128)
saved_i128 = [I128.lowest, I128.highest].iter()

check_i128 : I128, I128, Str -> Bool
check_i128 = |low, high, prefix| {
	items = List.with_capacity(4).append(low).append(high).append(low)
	slice = items.sublist({ start: 1, len: 2 })
	changed = items.set(0, high)
	grown = items.append(high)
	changed_slice = slice.set(1, high)
	grown_slice = slice.append(high)
	names = items.map(|item| prefix.concat(Str.inspect(item)))
	items == [low, high, low]
		and slice == [high, low]
		and changed == Ok([high, high, low])
		and grown == [low, high, low, high]
		and changed_slice == Ok([high, high])
		and grown_slice == [high, low, high]
		and items.get(0) == Ok(low)
		and items.get(1) == Ok(high)
		and items.get(3) == Err(OutOfBounds)
		and items.set(3, high) == Err(OutOfBounds)
		and items.first() == Ok(low)
		and items.last() == Ok(low)
		and items.contains(high)
		and items.concat(slice) == [low, high, low, high, low]
		and second(items) == Ok(high)
		and items.keep_if(|item| item == high) == [high]
		and grown.map(|item| item) == [low, high, low, high]
		and items.iter_rev().fold([], |acc, item| acc.append(item)) == [low, high, low]
		and saved_i128.fold([], |acc, item| acc.append(item)) == [I128.lowest, I128.highest]
		and items.fold(0, |count, _item| count + 1) == 3
		and names == [prefix.concat(Str.inspect(low)), prefix.concat(Str.inspect(high)), prefix.concat(Str.inspect(low))]
}

expect check_i128(I128.lowest, I128.highest, "a captured prefix longer than an inline string: ")
