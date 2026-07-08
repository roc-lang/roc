import AbiLayout exposing [AbiLayout]
import HostRcPlan exposing [HostRcPlan]
import TypeRepr exposing [TypeRepr]

## One public glue type table row.
##
## Authoritative compiler sources:
## - `repr` is reflected checked type shape emitted by src/glue/glue.zig.
## - `layout` is the exact requested LIR layout for the same checked type id
##   from src/lir/program.zig and src/layout/store.zig.
## - `rc` is emitted from layout store refcount facts, not target-language
##   traversal.
TypeInfo := { layout : AbiLayout, rc : HostRcPlan, repr : TypeRepr }
