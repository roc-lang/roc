#!/usr/bin/env perl

use strict;
use warnings;

use Cwd qw(realpath);
use FindBin qw($Bin);
use File::Find qw(find);
use File::Spec;

my $ROOT = realpath(File::Spec->catdir($Bin, '..'));

sub ident {
    my (@parts) = @_;
    my $name = join('_', @parts);
    return qr/\b\Q$name\E\b/;
}

sub camel {
    my (@parts) = @_;
    my $name = join('', @parts);
    return qr/\b\Q$name\E\b/;
}

my @RULES = (
    { category => 'publication', regex => qr/\bcanonicalizePublished(?:Inner)?\b/, allowed => {} },
    { category => 'resolved-canonicalization', regex => qr/\bcanonicalizeResolved(?:Inner)?\b/, allowed => {} },
    { category => 'graph-clone', regex => qr/\bcloneTypeGraph(?:Inner)?\b/, allowed => {} },
    { category => 'layout-facts-file', regex => qr/\blayout_facts\b/, allowed => {} },
    { category => 'layout-facts-export', regex => qr/\bLayoutFacts\b/, allowed => {} },
    { category => 'published-layout-finalizer', regex => qr/\bfinalizePublishedTypes\b/, allowed => {} },
    { category => 'old-clone-inst-file', regex => qr/\btype_clone_source\b/, allowed => {} },
    { category => 'workspace-root', regex => qr/\bprepareScopedFunctionRoot\b/, allowed => {} },
    { category => 'workspace-bind', regex => qr/\bbindSourceVarToExistingWorkspace\b/, allowed => {} },
    { category => 'workspace-ret', regex => qr/\blookupFunctionNodeRetVar\b/, allowed => {} },
    { category => 'workspace-curried-ret', regex => qr/\blookupCurriedFunctionFinalRetVar\b/, allowed => {} },
    { category => 'workspace-call-result', regex => qr/\bmaterializeAppliedFunctionResultVar\b/, allowed => {} },
    { category => 'workspace-align', regex => qr/\balignSourceVarWithWorkspaceVar\b/, allowed => {} },
    { category => 'workspace-bind-content', regex => qr/\bbindSourceContentToExistingWorkspace\b/, allowed => {} },
    { category => 'workspace-materialize-content', regex => qr/\bmaterializeSourceContentIntoWorkspaceVar\b/, allowed => {} },
    { category => 'workspace-merge-flex', regex => qr/\bmergeSourceFlexLikeIntoWorkspaceVar\b/, allowed => {} },
    { category => 'workspace-compute-call-result', regex => qr/\bcomputeAppliedFunctionResultVar\b/, allowed => {} },
    { category => 'module-name-scan', regex => qr/\bfindModuleIdxByName\b/, allowed => {} },
    { category => 'nominal-identity-wrapper', regex => qr/\bresolveNominalDefiningIdentity\b/, allowed => {} },
    { category => 'canonical-source-lookup', regex => qr/\blookupFnByCanonicalSource\b/, allowed => {} },
    { category => 'text-def-lookup-outside-typed-cir', regex => qr/\btopLevelDefByText\b/, allowed => { 'src/check/typed_cir.zig' => 1 } },
    { category => 'root-declaration-scan', regex => qr/\bfindDefByAssignedName\b/, allowed => {} },
    { category => 'root-declaration-scan', regex => qr/\bfindTopLevelDefByIdent\b/, allowed => {} },
    { category => 'root-declaration-scan', regex => qr/\bfindTopLevelDefByText\b/, allowed => {} },
    { category => 'shared-memory-fallback', regex => qr/\bcreateSharedMemoryWithFallback\b/, allowed => {} },
    { category => 'shared-memory-fallback', regex => qr/\bSHARED_MEMORY_FALLBACK_SIZE\b/, allowed => {} },
    { category => 'lir-module-env-boundary', regex => qr/\bcollectModuleEnvViews\b/, allowed => {} },
    { category => 'raw-provides-scan-after-publication', regex => qr/\bprovides_entries\.items\.items\b/, allowed => { 'src/check/checked_artifact.zig' => 1, 'src/check/Check.zig' => 1, 'src/canonicalize/test/exposed_shadowing_test.zig' => 1 } },
    { category => 'raw-const-template-mir-value', regex => qr/\bconst_ref:\s*(?:check\.CheckedArtifact\.|checked_artifact\.)ConstRef\b/, allowed => {} },
    { category => 'text-method-lookup-outside-typed-cir', regex => qr/\bresolveAttachedMethodTargetByText\b/, allowed => { 'src/check/typed_cir.zig' => 1 } },
    { category => 'text-ident-lookup-outside-typed-cir', regex => qr/\bfindCommonIdent\b/, allowed => { 'src/check/typed_cir.zig' => 1 } },
    { category => 'nullable-recorded-dispatch-lowering', regex => qr/\)\s*std\.mem\.Allocator\.Error!\?LoweredCall\s*\{/, allowed => {} },
    { category => 'nullable-attached-method-target', regex => qr/\)\s*std\.mem\.Allocator\.Error!\?ResolvedTarget\s*\{/, allowed => {} },
    { category => 'runtime-error-wrapper', regex => qr/\bmakeRuntimeErrorExprAt\b/, allowed => {} },
    { category => 'monotype-source-fn-arg-walk', regex => qr/\blookupCurriedFunctionArgVarInStore\(typed_cir_module\.typeStoreConst\(\),/, allowed => {} },
    { category => 'monotype-source-fn-ret-walk', regex => qr/\blookupFunctionRetVarInStore\(typed_cir_module\.typeStoreConst\(\),/, allowed => {} },
    { category => 'monotype-source-fn-arity-walk', regex => qr/\bfunctionArgCountInStore\(typed_cir_module\.typeStoreConst\(\),/, allowed => {} },
    { category => 'monotype-source-curried-result-walk', regex => qr/\blookupCurriedFunctionResultVarInStore\(typed_cir_module\.typeStoreConst\(\),/, allowed => {} },
    { category => 'callable-owner-retired-carrier', regex => ident(qw(exact fn symbol)), allowed => {} },
    { category => 'callable-owner-retired-carrier', regex => ident(qw(capture exact symbols)), allowed => {} },
    { category => 'callable-owner-retired-carrier', regex => ident(qw(arg exact symbols)), allowed => {} },
    { category => 'callable-owner-retired-carrier', regex => ident(qw(requested capture source tys)), allowed => {} },
    { category => 'callable-owner-retired-carrier', regex => ident(qw(capture source tys)), allowed => {} },
    { category => 'callable-owner-retired-carrier', regex => ident(qw(exact callable capture symbols)), allowed => {} },
    { category => 'callable-owner-retired-carrier', regex => ident(qw(exact callable capture symbols by symbol)), allowed => {} },
    { category => 'callable-owner-retired-carrier', regex => ident(qw(scoped exact callable capture symbols)), allowed => {} },
    { category => 'callable-owner-retired-carrier', regex => camel(qw(current Exact Callable Capture Symbols)), allowed => {} },
    { category => 'callable-owner-retired-carrier', regex => camel(qw(current Capture Payload From Symbols)), allowed => {} },
    { category => 'callable-owner-retired-carrier', regex => camel(qw(lookup Exact Callable Capture Symbols)), allowed => {} },
    { category => 'callable-owner-retired-carrier', regex => camel(qw(capture Exact Symbols From Env)), allowed => {} },
    { category => 'callable-owner-retired-carrier', regex => camel(qw(exact Callable Symbol From Source Type)), allowed => {} },
    { category => 'callable-owner-retired-carrier', regex => camel(qw(exact Callable Symbol For Bound Expr)), allowed => {} },
    { category => 'callable-owner-retired-carrier', regex => camel(qw(exact Callable Capture Count)), allowed => {} },
    { category => 'callable-owner-retired-carrier', regex => camel(qw(exact Callable Capture Symbols)), allowed => {} },
    { category => 'callable-owner-retired-carrier', regex => camel(qw(callable Facts For Solved Args)), allowed => {} },
    { category => 'callable-owner-retired-carrier', regex => camel(qw(register Scoped Exact Callable Capture Symbols)), allowed => {} },
    { category => 'investigation-trace', regex => qr/\bTRACE\b/, allowed => {} },
    { category => 'source-exec-retired-carrier', regex => qr/\bPlannedExec[A-Za-z0-9_]*\b/, allowed => {} },
    { category => 'source-exec-retired-carrier', regex => camel(qw(collect Planned Exec Bindings)), allowed => {} },
    { category => 'source-exec-retired-carrier', regex => camel(qw(plan Executable Type From Solved With Bindings)), allowed => {} },
    { category => 'source-exec-retired-carrier', regex => camel(qw(current Required Return Exec Ty)), allowed => {} },
    { category => 'source-type-reconstruction', regex => qr/\bexactTagSourceTypeForExpr\b/, allowed => {} },
    { category => 'source-type-reconstruction', regex => camel(qw(exact Tag Source Type For Expr)), allowed => {} },
    { category => 'promoted-wrapper-bridge-retired-carrier', regex => qr/\bPromotedWrapperBridge[A-Za-z0-9_]*\b/, allowed => {} },
    { category => 'promoted-wrapper-bridge-retired-carrier', regex => qr/\bpromoted_wrapper_bridges\b/, allowed => {} },
    { category => 'promoted-wrapper-bridge-retired-carrier', regex => qr/\barg_bridges\b/, allowed => {} },
    { category => 'promoted-wrapper-bridge-retired-carrier', regex => qr/\blowerPublishedPromotedWrapperBridge\b/, allowed => {} },
    { category => 'promoted-wrapper-bridge-retired-carrier', regex => qr/\blowerPromotedWrapperBridge[A-Za-z0-9_]*\b/, allowed => {} },
);

sub iter_zig_files {
    my @files;

    find(
        {
            no_chdir => 1,
            wanted   => sub {
                return unless $_ =~ /\.zig\z/;
                return if $File::Find::name =~ m{(?:^|/)\.zig-cache/};
                push @files, File::Spec->abs2rel($File::Find::name, $ROOT);
            },
        },
        File::Spec->catdir($ROOT, 'src'),
        File::Spec->catdir($ROOT, 'test'),
    );

    return sort @files;
}

my @violations;

for my $rel (iter_zig_files()) {
    my $path = File::Spec->catfile($ROOT, $rel);
    open my $fh, '<', $path or die "failed to read $rel: $!\n";

    my $line_no = 0;
    while (my $line = <$fh>) {
        ++$line_no;
        chomp $line;

        for my $rule (@RULES) {
            next if $rule->{allowed}{$rel};
            if ($line =~ $rule->{regex}) {
                push @violations, "$rel:$line_no: $rule->{category}: $line";
            }
        }
    }

    close $fh or die "failed to close $rel: $!\n";
}

sub brace_delta {
    my ($line) = @_;
    my $opens = ($line =~ tr/{/{/);
    my $closes = ($line =~ tr/}/}/);
    return $opens - $closes;
}

sub check_body_context_output_access {
    my $rel = 'src/postcheck/monotype/lower.zig';
    my $path = File::Spec->catfile($ROOT, $rel);
    open my $fh, '<', $path or die "failed to read $rel: $!\n";

    my %allowed_fn = map { $_ => 1 } qw(
        addExpr
        addPat
        addLocal
        addLocalWithBinder
        addFn
        reserveDef
        setDef
        addExprSpan
        addPatSpan
        addTypedLocalSpan
        addStmt
        addStmtSpan
        addFieldExprSpan
        addRecordDestructSpan
        addBranchSpan
        addIfBranchSpan
        addStrPatternStepSpan
        addStringLiteral
        addStringView
        addComptimeSite
        exprLoc
        exprRegion
        exprType
        patData
        localType
    );

    my $direct_output = qr/self\.builder\.program\.(?:addExpr|addPat|addLocal|addLocalWithBinder|addFn|addExprSpan|addPatSpan|addTypedLocalSpan|addStmt|addStmtSpan|addFieldExprSpan|addRecordDestructSpan|addBranchSpan|addIfBranchSpan|addStrPatternStepSpan|addStringLiteral|addStringView|addComptimeSite|defs\.append|defs\.items|exprs\.items|pats\.items|locals\.items)\b/;

    my $in_body_context = 0;
    my $body_depth = 0;
    my $current_fn;
    my $fn_started = 0;
    my $fn_depth = 0;
    my $line_no = 0;

    while (my $line = <$fh>) {
        ++$line_no;
        chomp $line;

        if (!$in_body_context) {
            if ($line =~ /^\s*const\s+BodyContext\s*=\s*struct\s*\{/) {
                $in_body_context = 1;
                $body_depth = brace_delta($line);
            }
            next;
        }

        if (!defined $current_fn && $line =~ /^\s+fn\s+([A-Za-z0-9_]+)\b/) {
            $current_fn = $1;
            $fn_started = 0;
            $fn_depth = 0;
        }

        if ($line =~ $direct_output && !$allowed_fn{$current_fn // ''}) {
            push @violations, "$rel:$line_no: body-context-final-output: $line";
        }

        my $delta = brace_delta($line);
        $body_depth += $delta;

        if (defined $current_fn) {
            if (!$fn_started && $line =~ /\{/) {
                $fn_started = 1;
            }
            $fn_depth += $delta if $fn_started;
            if ($fn_started && $fn_depth <= 0) {
                undef $current_fn;
                $fn_started = 0;
                $fn_depth = 0;
            }
        }

        last if $body_depth <= 0;
    }

    close $fh or die "failed to close $rel: $!\n";
}

check_body_context_output_access();

sub check_active_body_draft_seal_access {
    my $rel = 'src/postcheck/monotype/lower.zig';
    my $path = File::Spec->catfile($ROOT, $rel);
    open my $fh, '<', $path or die "failed to read $rel: $!\n";

    my $current_fn;
    my $fn_started = 0;
    my $fn_depth = 0;
    my $in_test = 0;
    my $test_started = 0;
    my $test_depth = 0;
    my $line_no = 0;

    while (my $line = <$fh>) {
        ++$line_no;
        chomp $line;

        if (!$in_test && $line =~ /^\s*test\s+"/) {
            $in_test = 1;
            $test_started = 0;
            $test_depth = 0;
        }

        if (!$in_test && !defined $current_fn && $line =~ /^\s+fn\s+([A-Za-z0-9_]+)\b/) {
            $current_fn = $1;
            $fn_started = 0;
            $fn_depth = 0;
        }

        if (!$in_test && ($current_fn // '') ne 'sealActiveBodyDraft') {
            if ($line =~ /\b[A-Za-z_][A-Za-z0-9_]*\.sealCoreIntoProgram\(/) {
                push @violations, "$rel:$line_no: active-body-draft-seal-bypass: $line";
            }
            if ($line =~ /\b[A-Za-z_][A-Za-z0-9_]*\.markNestedReady\(/) {
                push @violations, "$rel:$line_no: active-body-draft-seal-bypass: $line";
            }
            if ($line =~ /\b[A-Za-z_][A-Za-z0-9_]*\.seal\(self,\s*graph,\s*&sealer/) {
                push @violations, "$rel:$line_no: active-body-draft-seal-bypass: $line";
            }
            if ($line =~ /\bBodyDraftStore\.finalIdOffsets\(self\.program\)/) {
                push @violations, "$rel:$line_no: active-body-draft-seal-bypass: $line";
            }
        }
        if (!$in_test && ($current_fn // '') ne 'activeTypeFromNode') {
            if ($line =~ /\bactiveTypeViewForNode\(/) {
                push @violations, "$rel:$line_no: active-graph-view-bypass: $line";
            }
        }

        my $delta = brace_delta($line);

        if ($in_test) {
            if (!$test_started && $line =~ /\{/) {
                $test_started = 1;
            }
            $test_depth += $delta if $test_started;
            if ($test_started && $test_depth <= 0) {
                $in_test = 0;
                $test_started = 0;
                $test_depth = 0;
            }
        }

        if (defined $current_fn) {
            if (!$fn_started && $line =~ /\{/) {
                $fn_started = 1;
            }
            $fn_depth += $delta if $fn_started;
            if ($fn_started && $fn_depth <= 0) {
                undef $current_fn;
                $fn_started = 0;
                $fn_depth = 0;
            }
        }
    }

    close $fh or die "failed to close $rel: $!\n";
}

check_active_body_draft_seal_access();

if (@violations) {
    print "Post-check architecture violations found:\n";
    print "$_\n" for @violations;
    exit 1;
}

print "Post-check architecture check passed.\n";
exit 0;
