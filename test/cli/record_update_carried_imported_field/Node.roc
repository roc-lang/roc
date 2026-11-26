Node := [TypeName(Node.TypeName)].{
	TypeName : { flag : Bool, text : Str }

	type_name_of : Node -> Node.TypeName
	type_name_of = |n|
		match n {
			TypeName(r) => r
		}
}
