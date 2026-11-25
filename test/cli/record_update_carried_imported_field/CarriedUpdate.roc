# A record update whose base is the payload of an imported nominal, read back
# from a `var` that a call initialized. The carried `text` field's result slot
# and the base's field come from different checked instantiations of the same
# type; specialized lowering must give them one representation.
import Node

CarriedUpdate :: [].{}

make : {} -> Node
make = |_| Node.TypeName({ flag: Bool.False, text: "x" })

set_flag : {} -> Node
set_flag = |_| {
	var $result = make({})
	$result = Node.TypeName({ ..Node.type_name_of($result), flag: Bool.True })
	$result
}

expect {
	updated = Node.type_name_of(set_flag({}))
	updated.flag == Bool.True and updated.text == "x"
}
