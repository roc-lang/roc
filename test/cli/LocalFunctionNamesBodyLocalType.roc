LocalFunctionNamesBodyLocalType :: [].{}

# A local function whose annotation names a type declared in the same body and
# no type variable, where that type's backing names the body's type variable.
# Matching the type's tags inside the function must leave the enclosing
# function generic over that variable.
count : a, U64 -> U64
count = |x, n| {
	Local := [Default, Other(List(a))]
	size : Local -> U64
	size = |l| match l {
		Default => List.len([x])
		Other(items) => List.len(items)
	}
	if n == 0 { size(Local.Default) } else { size(Local.Other(List.repeat(x, n))) }
}

# A local function that returns the type, matched in the enclosing body.
first_or : a, a -> a
first_or = |x, fallback| {
	Local := [Default, Other(List(a))]
	wrap : List(a) -> Local
	wrap = |items| if List.is_empty(items) { Local.Default } else { Local.Other(items) }
	match wrap([x]) {
		Default => fallback
		Other(items) => match List.first(items) {
			Ok(item) => item
			Err(_) => fallback
		}
	}
}

expect count(5.U64, 0) == 1
expect count("s", 3) == 3
expect count([1.U8], 2) == 2
expect first_or("a", "b") == "a"
expect first_or(7.U64, 9) == 7
