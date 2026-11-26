# A polymorphic constant read at an open type inside a generic function its
# importers call at their own closed types.
PolyConstDep := [].{
	empty_items : List(_b)
	empty_items = []

	wrap : () -> List(_b)
	wrap = || empty_items
}
