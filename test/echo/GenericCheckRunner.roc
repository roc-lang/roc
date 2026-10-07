# Support module for rejected_requirement_via_imported_generic_dispatch.roc:
# a generic helper whose dispatch target the importer selects.
GenericCheckRunner := {}.{
	run = |w| w.check()
}
