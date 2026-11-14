import AbiLayout
import HostRcPlan
import TypeRepr

## One public glue type table row.
##
## Authoritative compiler sources:
## - `repr` is reflected checked type shape emitted by src/glue/glue.zig.
## - `layout` is the exact requested LIR layout for the same checked type id
##   from src/lir/program.zig and src/layout/store.zig.
## - `rc` is emitted from layout store refcount facts, not target-language
##   traversal.
## - `shape_id` is the id of the first row describing the same host type. Rows
##   with equal shape ids agree on repr, layout and ownership at every depth,
##   so one declaration serves all of them; rows that share a name but not a
##   shape id are different host types and need different names.
TypeInfo := { layout : AbiLayout, rc : HostRcPlan, repr : TypeRepr, shape_id : U64 }
