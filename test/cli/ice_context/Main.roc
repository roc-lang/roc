# Fixture for the crash-report tests: a Debug compiler started with
# ROC_DEBUG_FORCE_ICE panics on purpose in the named phase and module.
import Helper

main! = |_args| {
	echo!(Helper.greeting("ICE"))
	Ok({})
}
