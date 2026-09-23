app [main!] { pf: platform "../../fx/platform/main.roc" }

import pf.Stdout
import pf.NotAPublicModule

# `pf.NotAPublicModule` names no public module of the platform, so import
# resolution rejects it. The rejection is a user diagnostic: this module still
# checks, this call is checked-error data, and `main!` below still runs.
never_reached! = |{}| NotAPublicModule.line!("never reached")

main! = || Stdout.line!("entrypoint ran")
