# Completed Plan

## Goal

Finish the `cor`-style settle/freeze split in monotype and remove the
downstream runtime-shape bridge escape hatches.

## Completed Work

1. Split monotype into settle/freeze phases.
   - mutable specialization work stays inside `TypeCloneScope`
   - typed semantic facts freeze only after specialization settles
   - ordinary monotype lowering consumes frozen facts only

2. Removed ordinary-lowering `lowerInstantiatedType(...)` use.
   - remaining uses are freeze-time / builder-owned only

3. Moved downstream bridge decisions onto explicit facts.
   - LIR now carries explicit `list_reinterpret`
   - `for_list` carries explicit iterable element layout
   - dev codegen treats LIR control flow as a graph, not a tree

4. Removed active runtime-shape comparison from the pipeline.
   - `lambdamono` layout finalization now publishes explicit
     runtime-representation class ids for logical layout refs
   - IR carries those class ids forward
   - `FromIr` consumes class-id equality instead of structurally comparing
     finalized layouts
   - the old shared finalized-layout runtime comparer has been deleted

## End State

- no live `reinfer` offenders in the active compiler pipeline
- no live `reintern` offenders in the active compiler pipeline
- remaining builder-owned finalization inside monotype and `lambdamono`
  stays intentionally excluded
