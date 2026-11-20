## Dependency module for InspectUnpinnedTopLevelValue.roc: exposes values whose
## types keep a part that nothing pins, and one rendered inside this module.
InspectUnpinnedTopLevelValueDep := {}.{
	empty = []
	pair = ([], Ok([]))
	shown = Str.inspect(empty)
}
