#!/usr/bin/env perl
# Reunify migration manifest (reunify.md, Slice 0).
#
# Pins the exact per-file line counts of every mechanism by which the
# Monotype stage re-derives logical typing conclusions after checking:
# instantiation-graph creation, logical unification, the logical-graph import
# and point-in-time read boundary, deferred template requests, specialization
# request refinement, method-registry lookups for compiler-generated call
# edges, and the Lambda Solved special-relation census (reunify.md sections
# 6.6-6.7, 12.4).
#
# The gate fails when:
#   - a pinned pattern's line count in a file differs from the manifest
#     (growth is an unclassified addition; shrinkage is a stale entry that
#     must be updated in the same change); or
#   - a pinned pattern appears in a scanned file with no manifest entry.
#
# Counts may only shrink or move into a declared replacement category as
# the reunify migration proceeds. Raising a count requires editing this
# manifest in the same PR, which is the review ratchet. There is no
# warning mode.
#
# The pins below were re-taken wholesale when origin/main's Monotype
# restructuring merged in. That change replaced mutable Monotype views with
# immutable snapshots, deleted deferred template requests outright, and gave
# each specialization an independent graph, so several pinned mechanisms went to
# zero and were removed from this manifest: the point-in-time read API
# (`pointInTimeTypeForNode`, `registerNodeType`, `importMonoInner`), the whole
# deferred-template category (`DeferredTemplate`, `deferred_templates`,
# `drainSpecRequests*`, `sealDeferredSpecRequestsFrom`,
# `pinDeferredTemplateRequestToCheckedRoot`, `unsolved_monos`), the
# component-scoped registry lookups (`componentMethodTargetByName`,
# `findComponentMethodTargetByName`, `componentMethodTargetForView`,
# `componentDispatchOwner`), and the Lambda Solved score-selection census
# (`generatedBackingScore`, `generatedOpaqueEvidenceScore`,
# `isScoreSelectedEvidenceOwner`, `unifyIteratorBackings`, `isEmptyTagUnion`,
# `expectExprAtTypeEvenIfDone`, `expectGeneratedIteratorBackingExpr`,
# `in_iter_backing`, `forced_dynamic_backings`), which producer-authored backing
# authority replaced. Counts that rose did so because that merge grew the
# mechanism, not because this branch added call sites.
#
# Permanent carve-out: src/postcheck/lambda_solved/ keeps its unifier.
# Lambda sets exist nowhere in checked module data; Lambda Solved is their
# first derivation, not a re-derivation (reunify.md section 12; design.md
# "Lambda Solving"). Its census entries below pin the special relations of
# its structural walk so new special cases cannot land unclassified; they
# are not scheduled for deletion.

use strict;
use warnings;

my $SOLVE = 'src/postcheck/monotype/solve.zig';
my $LOWER = 'src/postcheck/monotype/lower.zig';
my $SPEC  = 'src/postcheck/monotype/specialize.zig';
my $LS    = 'src/postcheck/lambda_solved/solve.zig';
my $LSTY  = 'src/postcheck/lambda_solved/type.zig';
my $SLL   = 'src/postcheck/solved_lir_lower.zig';
my $LMLOW = 'src/postcheck/lambda_mono/lower.zig';
my $RPOL  = 'src/postcheck/representation_policy.zig';
my $RCLO  = 'src/postcheck/representation_closure.zig';
my $DTRAN = 'src/postcheck/monotype/direct_translate.zig';
my $PCMOD = 'src/postcheck/mod.zig';
my $FSID  = 'src/postcheck/monotype/final_spec_id.zig';
my $SREH  = 'src/postcheck/monotype/spec_rehearsal.zig';
my $MTYPE = 'src/postcheck/monotype/type.zig';

# Every category scans all .zig files under src/postcheck. `exempt` lists
# path prefixes whose matches are intentionally outside the manifest.
my @categories = (
    {
        name    => 'instantiation-graph-creation',
        exempt  => [],
        patterns => [
            { label => 'InstGraph.create(', re => qr/InstGraph\.create\(/,
              counts => { $SOLVE => 39, $LOWER => 18 } },
            { label => 'InstVariable.row(', re => qr/InstVariable\.row\(/,
              counts => { $SOLVE => 16, $LOWER => 3 } },
        ],
    },
    {
        name    => 'logical-unification',
        # Lambda Solved's unifier is the permanent callable-flow carve-out.
        exempt  => ['src/postcheck/lambda_solved/'],
        patterns => [
            #
            # Every one of $LOWER's 42 has a `census.UnifySite` identity
            # (reunify.md sections 9, 13 Slice 7), so the constraint-replay table
            # states the whole surface rather than the part that happens to carry
            # a hook. The mapping is not one member per call: 7 of the 42 sit
            # inside the shared relaters `relateFunctionRequestInterface`,
            # `relateRequestComponent` and `relateCheckedMonoRequestNodeAt`,
            # which state a relation named at their own call sites (the
            # `request_component_*`, `function_request_interface_*` and
            # `checked_mono_request_*` members), and 1 is a unit test. One call
            # carries two members, because `constrainTypeToCellAt` states both a
            # general checked-to-cell relation and the record-field read.
            #
            # $SOLVE's 19 are not constraint-replay sites: 11 are unit tests and
            # the other 8 are steps inside the unifier itself
            # (`unifyRecursiveFunctionInterface`,
            # `finalizeGeneratedIteratorRepresentations`,
            # `relateOpaqueInterfacePair`, `relateOpaqueChild`,
            # `unifyRowWithEmpty`), reached only while executing a relation a
            # $LOWER site already named. They delete with the unifier.
            # $PCMOD's single hit is a structural test asserting that the
            # string does NOT appear in the process source, not a call.
            { label => '.unify(', re => qr/\.unify\(/,
              counts => { $SOLVE => 21, $LOWER => 39, $PCMOD => 1 } },
            { label => 'unifyRoots', re => qr/\bunifyRoots\b/,
              counts => { $SOLVE => 2 } },
            { label => 'unifyConcrete', re => qr/\bunifyConcrete\b/,
              counts => { $SOLVE => 2 } },
            { label => 'unifyThroughBacking', re => qr/\bunifyThroughBacking\b/,
              counts => { $SOLVE => 18 } },
            { label => 'unifyTagRows', re => qr/\bunifyTagRows\b/,
              counts => { $SOLVE => 2 } },
            { label => 'unifyRecordRows', re => qr/\bunifyRecordRows\b/,
              counts => { $SOLVE => 2 } },
            { label => 'unifyRowWithEmpty', re => qr/\bunifyRowWithEmpty\b/,
              counts => { $SOLVE => 5 } },
            { label => 'writeOrQueueTagRest', re => qr/\bwriteOrQueueTagRest\b/,
              counts => { $SOLVE => 11 } },
            { label => 'writeOrQueueRecordRest', re => qr/\bwriteOrQueueRecordRest\b/,
              counts => { $SOLVE => 11 } },
        ],
    },
    {
        # The constraint-replay census (reunify.md sections 9, 13 Slice 7). The
        # `.unify(` pins above say how many relations body lowering still
        # replays; these pins say how many of them are measured. Both must move
        # together: when origin/main funnelled body lowering's relations through
        # shared relaters, the `.unify(` pins were re-taken while the hooks were
        # not, and 45 of 53 declared sites silently lost their only hook, so the
        # table read as "every execution redundant" over a tenth of the surface.
        # Pinning the hook counts makes that failure a gate failure.
        #
        # `measureUnifySite` is the two-sided measurement (one definition in
        # $SREH, one forwarding definition plus its call sites in $LOWER);
        # `noteUnifySite` is the same measurement for a site whose two sides are
        # both graph nodes; `noteUnifyConstruction` is the node-building class.
        # All three delete with the rehearsal at the flip.
        name    => 'constraint-replay-census',
        exempt  => [],
        patterns => [
        ],
    },
    {
        # The graph-read boundary: an external Monotype is imported as a node.
        # A solved node is read back only as an immutable snapshot, and no
        # visible id is mutated after it is read (see
        # ci/check_postcheck_architecture.pl for the deleted-API gate on the
        # mutable-view/refill API). These readers stay until Slice 7 deletes
        # logical graph solving.
        # $LOWER gained one `importMono` for the born-final callable: a ground
        # direct-call request instantiates as a constant of directed
        # translation's answer instead of an open instantiation node, so the
        # import REPLACES instNode work rather than adding a read of solved
        # state. It deletes with the graph, whose relations it only constrains.
        # A second $LOWER `importMono` is the same statement for ground
        # dispatch callables.
        name    => 'logical-graph-import',
        exempt  => [],
        patterns => [
            { label => 'importMono', re => qr/\bimportMono\b/,
              counts => { $SOLVE => 15, $LOWER => 22 } },
        ],
    },
    {
        name    => 'request-refinement',
        exempt  => [],
        patterns => [
            { label => 'refineRequest', re => qr/\brefineRequest\b/,
              counts => { $SPEC => 2 } },
            { label => 'appendAliasEntry', re => qr/\bappendAliasEntry\b/,
              counts => { $SPEC => 3 } },
            { label => 'RefinedDigestShadow', re => qr/\bRefinedDigestShadow\b/,
              counts => { $SPEC => 2 } },
            { label => 'refined_digest_shadow', re => qr/\brefined_digest_shadow\b/,
              counts => { $SPEC => 6 } },
            { label => 'reserved_identities', re => qr/\breserved_identities\b/,
              counts => { $SPEC => 7 } },
        ],
    },
    {
        name    => 'generated-edge-registry-lookup',
        exempt  => [],
        patterns => [
            { label => 'method_registry.lookup(', re => qr/method_registry\.lookup\(/,
              counts => { $LOWER => 1 } },
            { label => 'lookupMethodTargetByName', re => qr/\blookupMethodTargetByName\b/,
              counts => { $LOWER => 29 } },
            { label => 'findMethodTargetByName', re => qr/\bfindMethodTargetByName\b/,
              counts => { $LOWER => 2 } },
            { label => 'methodTargetInView', re => qr/\bmethodTargetInView\b/,
              counts => { $LOWER => 3 } },
            { label => 'ScopedMethodDispatch', re => qr/\bScopedMethodDispatch\b/,
              counts => { $LOWER => 4 } },
        ],
    },
    {
        name    => 'generated-evidence-synthesis',
        exempt  => [],
        patterns => [
            { label => 'synthesizeTargetEvidence', re => qr/\bsynthesizeTargetEvidence\b/,
              counts => { $LOWER => 2 } },
            { label => 'synthesizeParamsEvidence', re => qr/\bsynthesizeParamsEvidence\b/,
              counts => { $LOWER => 2 } },
            { label => 'synthesizeComponentEvidence', re => qr/\bsynthesizeComponentEvidence\b/,
              counts => { $LOWER => 3 } },
            { label => 'walkEvidencePath', re => qr/\bwalkEvidencePath\b/,
              counts => { $LOWER => 3 } },
        ],
    },
    {
        # The verified census of every decision Lambda Solved's structural
        # walk makes beyond callable slots (reunify.md section 12.4 item 5).
        # These pins freeze the census: a new special relation must be
        # classified here and in the census before it can land.
        name    => 'lambda-solved-census',
        exempt  => [],
        patterns => [
            { label => 'unifyGeneratedOpaqueBacking', re => qr/\bunifyGeneratedOpaqueBacking\b/,
              counts => { $LS => 2 } },
            { label => 'unifyIteratorOwnerStampedPublic', re => qr/\bunifyIteratorOwnerStampedPublic\b/,
              counts => { $LS => 2 } },
            { label => 'unifyForcedDynamicIterator', re => qr/\bunifyForcedDynamicIterator\b/,
              counts => { $LS => 2 } },
            { label => 'unifyGeneratedIteratorJoin', re => qr/\bunifyGeneratedIteratorJoin\b/,
              counts => { $LS => 2 } },
            { label => 'unifyPublicGeneratedIterator', re => qr/\bunifyPublicGeneratedIterator\b/,
              counts => { $LS => 2 } },
            { label => 'transparentAliasBacking', re => qr/\btransparentAliasBacking\b/,
              counts => { $LS => 4, $LOWER => 5 } },
            { label => 'mergeLambdaSets', re => qr/\bmergeLambdaSets\b/,
              counts => { $LS => 6 } },
            { label => 'unifyCaptures', re => qr/\bunifyCaptures\b/,
              counts => { $LS => 1 } },
            { label => 'markErasedCallablesReachedByType', re => qr/\bmarkErasedCallablesReachedByType\b/,
              counts => { $LS => 10 } },
            { label => 'markErasedCallablesReachedByTypeInner', re => qr/\bmarkErasedCallablesReachedByTypeInner\b/,
              counts => { $LS => 12 } },
            { label => 'closeCallableSlot', re => qr/\bcloseCallableSlot\b/,
              counts => { $LS => 3 } },
            { label => 'structuralBackingForNamed', re => qr/\bstructuralBackingForNamed\b/,
              counts => { $LS => 2 } },
            { label => 'active_unifications', re => qr/\bactive_unifications\b/,
              counts => { $LS => 8 } },
            { label => 'forall', re => qr/\bforall\b/,
              counts => { $LS => 12, $LSTY => 1, $SLL => 2, $LMLOW => 7 } },
        ],
    },
    {
        # The shared representation-relation policy (reunify.md section 10) and
        # the representation slot closure engine. Every call into the policy and
        # every `relate` in the closure engine cites a declared rule; these pins
        # start section 14's "every call cites a declared rule" mechanically
        # tracked. The policy entry points are pure functions over immutable
        # descriptors; the closure `relate` sites are the engine's own recursion
        # plus its direct tests (the engine is not yet wired into production).
        #
        # Both production stages classify representation relations through this
        # policy. `Type.iteratorRelation` in $MTYPE is the adapter that finished
        # named types reach it through, and Lambda Solved classifies through
        # that adapter; $SOLVE builds its own descriptors because the graph also
        # holds representations its producer has not sealed yet, and states each
        # one's minting identity and its own component answer. Named backings
        # are still selected by producer authority rather than by score, which
        # is why the evidence-selection pins below stay off $SOLVE and $LS.
        name    => 'representation-policy',
        exempt  => [],
        patterns => [
            # $SREH (Slice 7 flip-prep step b): a constraint-replay site whose two
            # sides are one logical type carrying two representations asks the
            # shared policy whether it declares a relation for the pair, so the
            # site is classified as a section 10 decision only where that policy
            # covers it. It deletes with the rehearsal at the flip.
            # $MTYPE holds the adapter every finished named type reaches the
            # policy through, and $SOLVE the graph's own call for the
            # representations it is still minting; both stay past the flip
            # because the tier relation is section 10's, not logical solving's.
            # $DTRAN (reunify.md section 10): directed translation EMITS the
            # representation at a position whose runtime encoding the checked data
            # does not dictate, so it asks the shared policy which section 10.3
            # rule relates the declared encoding to the one a producer stated
            # there. It is the flip's own call site and stays past the flip.
            { label => 'iteratorTierRelation', re => qr/\biteratorTierRelation\b/,
              counts => { $RPOL => 8, $RCLO => 1, $SREH => 1, $MTYPE => 1, $SOLVE => 1, $DTRAN => 1 } },
            { label => 'iteratorJoin', re => qr/\biteratorJoin\b/,
              counts => { $RPOL => 9, $RCLO => 2 } },
            { label => 'chooseGeneratedEvidenceBacking', re => qr/\bchooseGeneratedEvidenceBacking\b/,
              counts => { $RPOL => 4, $RCLO => 2 } },
            # $SREH (Slice 7 flip-prep step b): the per-specialization rehearsal
            # decides, rather than mirrors, the representation of each position
            # it emits, so it asks the shared policy which owners select their
            # backing by score before it builds that position's slot. The second
            # line asks the same question of a constraint-replay site's two
            # differing named heads. Both delete with the rehearsal at the flip.
            # $DTRAN asks the same question twice while emitting: once for the
            # position it is opening a representation slot at, and once for an
            # already-emitted child it models as that slot's component. Both stay
            # past the flip.
            { label => 'evidenceOwnerUsesScoreSelection', re => qr/\bevidenceOwnerUsesScoreSelection\b/,
              counts => { $RPOL => 7, $SREH => 2, $DTRAN => 2 } },
            # $FSID gained one `relate(` in Slice 7 Stage C: the production
            # FinalSpecId computation seals a record's representation inputs by
            # relating same-logical positions through the section 10.3 closure
            # engine, the same seal the shadow census runs. It is representation
            # closure, not logical re-derivation, and the flip keeps it.
            # $SREH gained one `relate(` in Slice 7 flip-prep step (b): the
            # per-specialization rehearsal relates the request context's emission
            # of a requesting edge to the callee's scheme root emitted under the
            # binding, which is that specialization's representation interface
            # edge (reunify.md 10.3, 11.1). It deletes with the rehearsal.
            # $SREH gained a second `relate(` for the draft layer's join: a
            # joinable slot the provisional walk opened at an undictated
            # position relates to the emitted tree's slot for the same position
            # under the public-meets-minted rule, which is how the slot learns
            # the final the draft seal projects (reunify.md 10.2, 10.6). It is
            # representation closure and stays past the flip.
            # $DTRAN gained one `relate(` in the section 10 emission layer: a
            # position whose runtime encoding the checked data does not dictate
            # relates its declared encoding to the one a producer stated there
            # under the rule the policy classified, and reads the sealed
            # representation back out of the resulting class. It is the flip's own
            # representation closure and stays past the flip.
            { label => 'relate(', re => qr/\brelate\(/,
              counts => { $RCLO => 18, $FSID => 1, $SREH => 2, $DTRAN => 1 } },
            { label => 'relateNominalBacking', re => qr/\brelateNominalBacking\b/,
              counts => { $RCLO => 4 } },
        ],
    },
    {
        # The reunify.md Slice 7 flip staging, Stage A: the directed stored-form
        # translation relocated into a production Monotype module as inert code,
        # plus the Debug equality probe that exercises it. Like the shadow, this
        # surface grows as the later stages wire it in; its pins move up in the
        # same change that grows it. Stage E repoints the production lowering seam
        # onto `translateGroundRoot`/`instantiateStoredScheme` and deletes the
        # graph, at which point these entry points stop being inert.
        name    => 'direct-translation',
        exempt  => [],
        patterns => [
            { label => 'instantiateStoredScheme', re => qr/\binstantiateStoredScheme\b/,
              counts => { $DTRAN => 4 } },
        ],
    },
);

my $scan_root = 'src/postcheck';

# `structural_test.zig` embeds the post-check sources and asserts on their text,
# so every pinned identifier appears there as a string literal rather than as a
# call site. Counting those literals would pin the assertions instead of the
# mechanisms, so the scan skips this file in every category.
my @GLOBAL_EXEMPT = ('src/postcheck/structural_test.zig');

sub zig_files_under {
    my ($root) = @_;
    my @files;
    my @queue = ($root);
    while (@queue) {
        my $dir = shift @queue;
        opendir(my $dh, $dir) or die "cannot open $dir: $!";
        for my $entry (sort readdir($dh)) {
            next if $entry eq '.' || $entry eq '..';
            my $path = "$dir/$entry";
            if (-d $path) {
                next if $entry eq '.zig-cache';
                push @queue, $path;
            } elsif ($entry =~ /\.zig$/) {
                push @files, $path;
            }
        }
        closedir($dh);
    }
    return sort @files;
}

my @violations;
my @files = zig_files_under($scan_root);

my %file_lines;
for my $file (@files) {
    open(my $fh, '<', $file) or die "cannot read $file: $!";
    my @lines = <$fh>;
    close($fh);
    $file_lines{$file} = \@lines;
}

for my $category (@categories) {
    for my $pattern (@{ $category->{patterns} }) {
        my %seen;
        FILE: for my $file (@files) {
            for my $prefix (@{ $category->{exempt} }, @GLOBAL_EXEMPT) {
                next FILE if index($file, $prefix) == 0;
            }
            my $count = 0;
            for my $line (@{ $file_lines{$file} }) {
                $count++ if $line =~ $pattern->{re};
            }
            $seen{$file} = $count if $count > 0;
            my $expected = $pattern->{counts}{$file} // 0;
            if ($count != $expected) {
                my $direction = $count > $expected
                    ? "grew ($expected -> $count): unclassified addition; shrink it or classify it in this manifest"
                    : "shrank ($expected -> $count): stale pin; lower the pinned count in this manifest";
                push @violations,
                    "$category->{name}: '$pattern->{label}' in $file $direction";
            }
        }
        for my $pinned (sort keys %{ $pattern->{counts} }) {
            next if exists $seen{$pinned};
            my $expected = $pattern->{counts}{$pinned};
            next if $expected == 0;
            push @violations,
                "$category->{name}: '$pattern->{label}' pinned at $expected in $pinned but the file has no matches (stale pin or missing file)";
        }
    }
}

if (@violations) {
    print "Reunify manifest violations found:\n";
    print "  $_\n" for @violations;
    print "\nTotal: ", scalar(@violations), " violation(s).\n";
    print "The manifest is ci/check_reunify_manifest.pl; reunify.md Slice 0 defines the policy.\n";
    exit 1;
}

print "Reunify manifest check passed.\n";
exit 0;
