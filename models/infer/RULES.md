# infer rules

The model is Hindley-Milner inference with Rémy-style levels, extensible tag and record rows, and the Roc rules below. Each
rule name appears as a comment beside the code that implements it (`ref.py`, `lang.py`). Compiler sites are function names in
`src/check/Check.zig` unless another file is named; "none" means the compiler does not implement the rule, and the linked issue
says so.

## The fragment

- **Expressions:** variables, `N.U64` literals, `Bool.True` / `Bool.False`, tags with payloads, closed record literals, field
  access, lambdas, calls, `if`, `match` (tag, variable and `_` patterns, nested tag sub-patterns), blocks with local
  definitions (annotated or not), `e?`, list literals.
- **Types:** `U64`, `Bool`, `List(a)`, `Try(ok, err)` (a nominal over `[Ok(ok), Err(err)]`), functions with an effect kind,
  tag rows, record rows.
- **Annotations:** named variables, `_`, implicit and explicit `..` / `..r`, `->` and `=>`.
- **Effects:** one annotated primitive, `e! : U64 => U64`.

## Rules

| Rule | What it says | design.md section | Compiler site |
|---|---|---|---|
| R-GenByExpr | Only a lambda or a bare reference (value alias) on the right-hand side generalizes. Every other binding is weak. | Value Bindings Generalize By Expression | `shouldGeneralize` |
| R-ValueAnn | An annotation on a weak binding that introduces a type variable (named, or `..`) is rejected. A name looked up from an enclosing annotation is not introduced. | Value Bindings Generalize By Expression | `rejectTopLevelValueAnnotations`, `rejected_value_annotations` |
| R-ScopedTypeVars | A local annotation sees the type variables of enclosing annotations. | Value Bindings Generalize By Expression | `rigidVarOwner`, `noteRigidVarLookupForLocalProcedures` |
| R-SeedParams | An annotated lambda's parameter patterns are typed against the annotation's argument types before the body. | Row Subsumption | `resumeLambdaCheck` |
| R-Polarity | In an annotation, an extensionless non-empty union in a positive position gets a fresh bounded flex extension. An explicit `..` in a positive position means the same. Negative positions stay closed. `[]` is exempt. | Polarity: Output-Position Tag Unions Are Implicitly Open | `stepTagUnionAnnoGen`, `GenTypeAnnoCtx.polarityVarBehavior` |
| R-Bounded | A bounded extension may close or merge with a flex, never gain a tag. | Polarity: Output-Position Tag Unions Are Implicitly Open | `bounded_row_ext` in `src/check/unify.zig`; `beginBoundedAnnotationRows` |
| R-BoundTravels | When a bounded extension merges with a flex, the bound travels with the merged class. | same | `src/check/unify.zig` |
| R-InstNoBound | Instantiating a scheme does not copy the bound. | same; Closed Result Rows At Template Boundaries | `src/types/instantiate.zig` |
| R-BoundedRigid | A bounded extension may not merge with a rigid tail: the rigid stands for tags the annotation does not list. | Polarity; Row Subsumption ("two identically annotated definitions have one type") | none ([#13297](https://github.com/roc-lang/roc/issues/13297)) |
| R-RigidEscape | A rigid may not be bound into a variable of an outer level, for example a top-level weak value's. | Row Subsumption (deferred result relations) | none ([#13298](https://github.com/roc-lang/roc/issues/13298)) |
| R-Subsume | Each result-position leaf of an annotated lambda's body (an `if` or `match` branch, a block's final value) relates to the annotated result through a copy in which each closed tag row is re-opened: the direct row, tag payloads, and `Try`'s error row. | Row Subsumption; Closed Result Rows At Template Boundaries | `widenedReachCopy`, `widenedClosedRowsCopy` |
| R-DeferredClose | A bounded row that an inner annotation shares with a variable the enclosing definition owns takes the annotation's row, closed, at the enclosing generalization boundary. | Row Subsumption (deferred result relations) | `reachedVarEscapesResultRelation` |
| R-MatchClose | A match's patterns are typed jointly per position. Without a catch-all at a position the row is closed with exactly the listed tags; with one it is open. | Pattern-Only Tags | not traced |
| R-KnownScrutinee | A match whose scrutinee has no runtime dependency is checked against its compile-time value. The model reports such a rejection as `uncertain` and does not compare it. | Checking Effects And Const Roots | not traced |
| R-TryBacking | `Try(a, e)` unifies with a structural row as `[Ok(a), Err(e)]`. A `Try` read as a row is its backing. | Nominal Constructor Backing Relation | `unifyNominalType`, `openNominalBacking` in `src/check/unify.zig` |
| R-TryCompose | A lambda with `?` has a composed result. A closed contribution adds its tags only. An open contribution with tags is equated with the result row. A tagless open contribution becomes the residual tail when unannotated, and is related to the whole declared row when annotated, unless it already is the declared residual tail. | Try Return-Row Composition | `gatherTryReturnRow`, `projectTryReturnRow`, `noteComposedBodyRelation` |
| R-Lacks | A label that a row tail turns out to carry merges with the same-named head when the payloads agree, and is a "conflicting tag" mismatch otherwise. | Try Return-Row Composition; Row Union Normalization | `firstRowLabelConflict`, `reportRowUnionConflict` |
| R-NoAnonRecursion | A structural type may not contain itself. | none | `src/check/occurs.zig` |
| R-RowKind | A variable used as a tag-row (or record-row) tail can only become a row of that kind. | none (kinds are implicit) | `src/check/unify.zig` |
| R-ValueRowBound | An annotated weak value's rows are the annotation's and no use widens them. | Value Rows: Uses Share One Row | `endBoundedAnnotationRows` |
| R-TopValueClose | Every still-open tag row of a top-level value closes when the module finishes. | Value Rows: Uses Share One Row | `closeTopLevelValueRows`, `closeWeakValueImplicitOpenExts` |
| R-EffectSlot | A lambda's effect kind is a fresh variable whose dependencies are the kinds of the calls in its body. A call adds the callee's kind to the active slot. Creating a function is not effectful. | Effect Slots | `recordCallEffect`, `recordCurrentFunctionEffectDependency` |
| R-PureAnnotation | Unifying a kind with `->` forces every dependency pure. | Effect Slots | `functionEffectState` |
| R-EffectfulValue | A top-level value whose slot resolves effectful is rejected. | Checking Effects And Const Roots | `finishDef` (`effectful_top_level`) |
| R-EffectFinalize | A source lambda whose kind resolves effectful prints `=>`. An instantiated kind with unresolved dependencies prints `->`, as the compiler does today ([#13299](https://github.com/roc-lang/roc/issues/13299)). | Effect Slots | `lambdaBodyIsEffectful`; `src/types/TypeWriter.zig` |

## Printing

The model prints types the way `src/types/TypeWriter.zig` does, so they can be compared with REPL `:t`:

- a positive-position anonymous flex extension that occurs once is hidden;
- a variable that occurs once prints `_x`;
- a function result that is a function is parenthesized.

`roc.canon` renames variables by first appearance and writes any variable that occurs once as `_`, however it is spelled (Roc
keeps source names on instantiated flex variables). When two types differ only in variable identity, the row is labeled
`type-names` ([#13300](https://github.com/roc-lang/roc/issues/13300)); any other difference is `type`.
