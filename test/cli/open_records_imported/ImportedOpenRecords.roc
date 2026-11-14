import OpenMaker

# An unannotated function in another module leaves its records open, so a use
# here closes them or makes them nominal types of this module with exactly
# their fields.

ImportedOpenRecords :: [].{}

Item := { key : U64, content : Str }.{
	show : Item -> Str
	show = |item| Str.concat(item.key.to_str(), item.content)
}

expect {
	item : Item
	item = OpenMaker.make(1)
	item.show() == "1m"
}

expect OpenMaker.make(2).content == "m"

expect {
	items : List(Item)
	items = OpenMaker.listed(3)
	items.map(Item.show) == ["3l"]
}

expect {
	item : Item
	item = { key: 4, content: "x" }
	OpenMaker.read(item) == "4x"
}
