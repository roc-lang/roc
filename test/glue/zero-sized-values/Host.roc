Host := [].{
	Empty := { z : {}, ready : [Ready] }

	args! : U8, [Ready], U16 => [Hollow({}, {})]
	ready! : Empty => Empty
	record! : { z : {}, s : Str } => U8
}
