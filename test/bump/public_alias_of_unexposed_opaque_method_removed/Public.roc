import Hidden

Public := [Doc(Public.Store)].{
	## Opaque attribute store, re-exported under this module's name.
	Store : Hidden.Store

	empty : Store
	empty = Hidden.Store.empty
}
