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
my $RSLOG = 'src/postcheck/reunify_shadow/logical_identity.zig';
my $RSHAD = 'src/postcheck/reunify_shadow/shadow.zig';

# Every category scans all .zig files under src/postcheck. `exempt` lists
# path prefixes whose matches are intentionally outside the manifest.
my @categories = (
    {
        name    => 'instantiation-graph-creation',
        exempt  => [],
        patterns => [
            { label => 'InstGraph.create(', re => qr/InstGraph\.create\(/,
              counts => { $SOLVE => 13, $LOWER => 14 } },
            { label => 'InstVariable.row(', re => qr/InstVariable\.row\(/,
              counts => { $SOLVE => 14, $LOWER => 1 } },
        ],
    },
    {
        name    => 'logical-unification',
        # Lambda Solved's unifier is the permanent callable-flow carve-out.
        exempt  => ['src/postcheck/lambda_solved/'],
        patterns => [
            { label => '.unify(', re => qr/\.unify\(/,
              counts => { $SOLVE => 14, $LOWER => 53 } },
            { label => 'unifyRoots', re => qr/\bunifyRoots\b/,
              counts => { $SOLVE => 2 } },
            { label => 'unifyConcrete', re => qr/\bunifyConcrete\b/,
              counts => { $SOLVE => 4 } },
            { label => 'unifyThroughBacking', re => qr/\bunifyThroughBacking\b/,
              counts => { $SOLVE => 18 } },
            { label => 'unifyTagRows', re => qr/\bunifyTagRows\b/,
              counts => { $SOLVE => 2 } },
            { label => 'unifyRecordRows', re => qr/\bunifyRecordRows\b/,
              counts => { $SOLVE => 2 } },
            { label => 'unifyRowWithEmpty', re => qr/\bunifyRowWithEmpty\b/,
              counts => { $SOLVE => 5 } },
            { label => 'writeOrQueueTagRest', re => qr/\bwriteOrQueueTagRest\b/,
              counts => { $SOLVE => 6 } },
            { label => 'writeOrQueueRecordRest', re => qr/\bwriteOrQueueRecordRest\b/,
              counts => { $SOLVE => 6 } },
        ],
    },
    {
        # The graph-read boundary: a solved graph node is read into an
        # immutable point-in-time Monotype id, and an external Monotype is
        # imported as a node. No visible id is mutated after it is read; the
        # mutable-view/refill API this category replaced was deleted in Slice 3
        # (see ci/check_postcheck_architecture.pl for the deleted-API gate).
        # These readers stay until Slice 7 deletes logical graph solving.
        name    => 'logical-graph-import',
        exempt  => [],
        patterns => [
            { label => 'importMono', re => qr/\bimportMono\b/,
              counts => { $SOLVE => 2, $LOWER => 43 } },
            { label => 'importMonoInner', re => qr/\bimportMonoInner\b/,
              counts => { $SOLVE => 9 } },
            { label => 'pointInTimeTypeForNode', re => qr/\bpointInTimeTypeForNode\b/,
              counts => { $SOLVE => 7, $LOWER => 2 } },
            { label => 'registerNodeType', re => qr/\bregisterNodeType\b/,
              counts => { $SOLVE => 1, $LOWER => 4 } },
        ],
    },
    {
        name    => 'deferred-template',
        exempt  => [],
        patterns => [
            { label => 'DeferredTemplate', re => qr/\bDeferredTemplate\b/,
              counts => { $SOLVE => 2 } },
            { label => 'deferred_templates', re => qr/\bdeferred_templates\b/,
              counts => { $SOLVE => 3, $LOWER => 7 } },
            { label => 'drainSpecRequests', re => qr/\bdrainSpecRequests\b/,
              counts => { $LOWER => 8 } },
            { label => 'drainSpecRequestsFrom', re => qr/\bdrainSpecRequestsFrom\b/,
              counts => { $LOWER => 3 } },
            { label => 'sealDeferredSpecRequestsFrom', re => qr/\bsealDeferredSpecRequestsFrom\b/,
              counts => { $LOWER => 2 } },
            { label => 'pinDeferredTemplateRequestToCheckedRoot', re => qr/\bpinDeferredTemplateRequestToCheckedRoot\b/,
              counts => { $LOWER => 3 } },
            { label => 'unsolved_monos', re => qr/\bunsolved_monos\b/,
              counts => { $SOLVE => 42, $LOWER => 33 } },
        ],
    },
    {
        name    => 'request-refinement',
        exempt  => [],
        patterns => [
            { label => 'refineRequest', re => qr/\brefineRequest\b/,
              counts => { $SPEC => 2, $LOWER => 2 } },
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
            { label => 'componentMethodTargetByName', re => qr/\bcomponentMethodTargetByName\b/,
              counts => { $LOWER => 11 } },
            { label => 'findComponentMethodTargetByName', re => qr/\bfindComponentMethodTargetByName\b/,
              counts => { $LOWER => 4 } },
            { label => 'componentMethodTargetForView', re => qr/\bcomponentMethodTargetForView\b/,
              counts => { $LOWER => 2 } },
            { label => 'lookupMethodTargetByName', re => qr/\blookupMethodTargetByName\b/,
              counts => { $LOWER => 2 } },
            { label => 'findMethodTargetByName', re => qr/\bfindMethodTargetByName\b/,
              counts => { $LOWER => 3 } },
            { label => 'methodTargetInView', re => qr/\bmethodTargetInView\b/,
              counts => { $LOWER => 3 } },
            { label => 'componentDispatchOwner', re => qr/\bcomponentDispatchOwner\b/,
              counts => { $LOWER => 5 } },
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
              counts => { $LOWER => 3 } },
            { label => 'synthesizeComponentEvidence', re => qr/\bsynthesizeComponentEvidence\b/,
              counts => { $LOWER => 2 } },
            { label => 'walkEvidencePath', re => qr/\bwalkEvidencePath\b/,
              counts => { $LOWER => 2 } },
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
            { label => 'unifyIteratorBackings', re => qr/\bunifyIteratorBackings\b/,
              counts => { $LS => 2 } },
            { label => 'transparentAliasBacking', re => qr/\btransparentAliasBacking\b/,
              counts => { $LS => 4 } },
            { label => 'isEmptyTagUnion', re => qr/\bisEmptyTagUnion\b/,
              counts => { $LS => 3 } },
            { label => 'mergeLambdaSets', re => qr/\bmergeLambdaSets\b/,
              counts => { $LS => 8 } },
            { label => 'unifyCaptures', re => qr/\bunifyCaptures\b/,
              counts => { $LS => 5 } },
            { label => 'markErasedCallablesReachedByType', re => qr/\bmarkErasedCallablesReachedByType\b/,
              counts => { $LS => 9 } },
            { label => 'markErasedCallablesReachedByTypeInner', re => qr/\bmarkErasedCallablesReachedByTypeInner\b/,
              counts => { $LS => 13 } },
            { label => 'closeCallableSlot', re => qr/\bcloseCallableSlot\b/,
              counts => { $LS => 3 } },
            { label => 'expectGeneratedIteratorBackingExpr', re => qr/\bexpectGeneratedIteratorBackingExpr\b/,
              counts => { $LS => 2 } },
            { label => 'expectExprAtTypeEvenIfDone', re => qr/\bexpectExprAtTypeEvenIfDone\b/,
              counts => { $LS => 17 } },
            { label => 'structuralBackingForNamed', re => qr/\bstructuralBackingForNamed\b/,
              counts => { $LS => 2 } },
            { label => 'generatedBackingScore', re => qr/\bgeneratedBackingScore\b/,
              counts => { $LS => 4 } },
            { label => 'generatedOpaqueEvidenceScore', re => qr/\bgeneratedOpaqueEvidenceScore\b/,
              counts => { $LS => 2 } },
            { label => 'isScoreSelectedEvidenceOwner', re => qr/\bisScoreSelectedEvidenceOwner\b/,
              counts => { $LS => 3 } },
            { label => 'in_iter_backing', re => qr/\bin_iter_backing\b/,
              counts => { $LS => 8 } },
            { label => 'forced_dynamic_backings', re => qr/\bforced_dynamic_backings\b/,
              counts => { $LS => 5 } },
            { label => 'active_unifications', re => qr/\bactive_unifications\b/,
              counts => { $LS => 7 } },
            { label => 'forall', re => qr/\bforall\b/,
              counts => { $LS => 7, $LSTY => 1, $SLL => 1, $LMLOW => 1 } },
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
        name    => 'representation-policy',
        exempt  => [],
        patterns => [
            { label => 'iteratorTierRelation', re => qr/\biteratorTierRelation\b/,
              counts => { $RPOL => 4, $RCLO => 1 } },
            { label => 'iteratorJoin', re => qr/\biteratorJoin\b/,
              counts => { $RPOL => 7, $RCLO => 2, $SOLVE => 1, $LS => 3 } },
            { label => 'chooseGeneratedEvidenceBacking', re => qr/\bchooseGeneratedEvidenceBacking\b/,
              counts => { $RPOL => 4, $RCLO => 2, $LS => 1 } },
            { label => 'evidenceOwnerUsesScoreSelection', re => qr/\bevidenceOwnerUsesScoreSelection\b/,
              counts => { $RPOL => 7, $LS => 1 } },
            { label => 'applyIteratorJoin', re => qr/\bapplyIteratorJoin\b/,
              counts => { $SOLVE => 2 } },
            # $RSHAD gained one `relate(` in Slice 6: the FinalSpecId sealing census
            # (reunify.md 11.1/11.2) seals each spec record's declared representation
            # inputs by relating same-logical positions through the section 10.3
            # closure engine, so the shadow drives real `relate` workload.
            { label => 'relate(', re => qr/\brelate\(/,
              counts => { $RCLO => 17, $RSHAD => 1 } },
            { label => 'relateNominalBacking', re => qr/\brelateNominalBacking\b/,
              counts => { $RCLO => 4 } },
        ],
    },
    {
        # The reunify.md Slice 5 shadow: eager logical identity, checked-node
        # translation, scheme instantiation, and the harness hook. Unlike the
        # shrinking re-derivation categories above, this shadow surface grows as
        # it expands to the full lifecycle (Slice 6); its pins move up in the
        # same change that grows it, so every entry point stays tracked.
        name    => 'reunify-shadow',
        exempt  => [],
        patterns => [
            { label => 'checkedLogicalIdentity', re => qr/\bcheckedLogicalIdentity\b/,
              counts => { $RSLOG => 18, $RSHAD => 1 } },
            { label => 'checkedLogicalIdentityUnder', re => qr/\bcheckedLogicalIdentityUnder\b/,
              counts => { $RSLOG => 10, $RSHAD => 2 } },
            # $RSHAD grew 1 -> 2 in Slice 6: the specialization-registry census
            # (reunify.md 11) reduces each sealed spec record's requested and solved
            # types to a logical skeleton through a second `monoLogicalIdentity` call
            # site (`logicalOf`), alongside the concrete-root comparison's call.
            { label => 'monoLogicalIdentity', re => qr/\bmonoLogicalIdentity\b/,
              counts => { $RSLOG => 2, $RSHAD => 2 } },
            # New in Slice 6: the representation-erasing sealing walk. It is defined
            # once in $RSLOG and called twice in the sealing census ($RSHAD) — once
            # for each spec record's requested type and once for its solved type —
            # to erase representation to the logical binding while collecting the
            # representation-input positions a FinalSpecId digests.
            { label => 'walkRequestSealing', re => qr/\bwalkRequestSealing\b/,
              counts => { $RSLOG => 1, $RSHAD => 2 } },
            { label => 'instantiateScheme', re => qr/\binstantiateScheme\b/,
              counts => { $RSLOG => 7, $RSHAD => 1 } },
            { label => 'alphaEqual', re => qr/\balphaEqual\b/,
              counts => { $RSLOG => 5, $RSHAD => 1 } },
            { label => 'runReunifyShadow', re => qr/\brunReunifyShadow\b/,
              counts => { $LOWER => 2 } },
        ],
    },
);

my $scan_root = 'src/postcheck';

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
            for my $prefix (@{ $category->{exempt} }) {
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
