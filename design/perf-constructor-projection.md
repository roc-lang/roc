# Constructor projection proposal

These are implementation notes for integration into `design.md`, not an
additional source of solver policy. The authoritative rules are Expected Shape
Context and Producer-Owned Single-Tag Construction.

## Expected tag context (F3)

A tag constructor supplies its syntactic identity and arity. Borrow its ordered
payload slots through aliases and row extensions, respecting nominal opacity
and declaration substitution. Copy selected slots together, preserving repeated
variable equalities. Declaration parameters denote owned copies of the actual
arguments, not mutable declaration-template cells. Context copies flex rigid
leaves and carry no executable dispatch or defaulting obligations.

Unknown tags, arity disagreement, or erroneous context establish no projection.
The enclosing full relation retains all omitted-variant constraints and owns
diagnostics. Stored payloads still instantiate before their relation to a
projected slot, and sibling payload checking retains its original order.

## Producer-owned constructor membership (F2, implementation in progress)

A nominal constructor proves membership in its declaration's backing union, not
equality between the backing expression's minimal structural type and every
alternative in that union. A syntax-owned single-tag construction can establish
that proof from declaration identity, actual arguments, tag identity, exact
arity, and the selected payload relation. All selected payloads are fresh owned
instances with ordinary declaration substitution and requirement bookkeeping.
The nominal application remains the authority for the complete union.

The backing expression keeps its sparse constructed type; absent declaration
variants are never inserted into its graph. No declaration-template cells become
mutable through this representation. This does not authorize dropping or
approximating the complement row of an arbitrary structural union: an observable
extension may be constrained elsewhere. Only the producer owns the fresh
construction extension. Context-only expected shapes cannot establish nominal
membership or monomorphize a scheme.

Consumers of backing expression and pattern types must obtain runtime
representation from the existing explicit
nominal construction edge and declaration identity, never by guessing from a
sparse solved type. Publication, evidence, diagnostics, LSP presentation, and
pattern analysis must agree. Rejection still needs the full declaration's
diagnostic shape, which may be materialized specifically for reporting.

The current expression implementation covers explicit nominal wrappers and
direct structural tag producers with an authoritative expected nominal type.
Nominal patterns use the same relation only for direct applied tags with a
fresh open row; closed-row patterns and patterns binding the whole backing
value retain the ordinary full relation. Arbitrary structural-row unification
is unchanged.
Rejection copies the full backing faithfully for diagnostic display, including
its attached constraints, without registering another use's dispatch/defaulting
obligations.

The audited Monotype expression relation uses the nominal graph's backing;
pattern lowering and binder registration likewise obtain the backing from the
outer nominal. Exhaustiveness converts nominal pattern wrappers to their
syntactic patterns. Publication preserves the nominal wrapper and its backing
edge. Documentation's nominal-child extraction is record-specific. Hover
formats the selected node's actual type, so an inner backing tag can now show
its sparse type while the outer constructor remains nominal.

The first targeted checker tranche passed with F2, F3, and F4 individually,
combined, and disabled, including recursive constructor patterns, selected
payload rejection, numeric/custom-quote optional payloads, and allocation
growth bounds. The existing nominal-filtered checker tests also passed with all
three flags enabled. Whole-corpus diagnostics, downstream runtime workflows,
and performance measurements remain separate validation obligations.

## Settled reachability scratch (F4)

The once-per-check settled-row reachability walk owns one visited set across
all roots and frees it after validation. Retaining its potentially module-sized
capacity in recurring small checker walks makes each later clear proportional
to the largest published graph. Isolation trades a separate temporary allocation
for bounded lifetime, without changing traversal order or row ownership.
