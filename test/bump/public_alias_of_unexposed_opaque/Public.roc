import Hidden

Public := [Doc(Public.Store)].{
	## Opaque attribute store, re-exported under this module's name.
	Store : Hidden.Store

	empty_store : Store
	empty_store = Hidden.Store.empty
}
