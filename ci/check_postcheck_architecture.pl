#!/usr/bin/env perl

use strict;
use warnings;

use Cwd qw(realpath);
use FindBin qw($Bin);
use File::Find qw(find);
use File::Spec;

my $ROOT = realpath(File::Spec->catdir($Bin, '..'));

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
    { category => 'text-method-lookup-outside-typed-cir', regex => qr/\bresolveAttachedMethodTargetByText\b/, allowed => { 'src/check/typed_cir.zig' => 1 } },
    { category => 'text-ident-lookup-outside-typed-cir', regex => qr/\bfindCommonIdent\b/, allowed => { 'src/check/typed_cir.zig' => 1 } },
    { category => 'nullable-recorded-dispatch-lowering', regex => qr/\)\s*std\.mem\.Allocator\.Error!\?LoweredCall\s*\{/, allowed => {} },
    { category => 'nullable-attached-method-target', regex => qr/\)\s*std\.mem\.Allocator\.Error!\?ResolvedTarget\s*\{/, allowed => {} },
    { category => 'runtime-error-wrapper', regex => qr/\bmakeRuntimeErrorExprAt\b/, allowed => {} },
    { category => 'monotype-source-fn-arg-walk', regex => qr/\blookupCurriedFunctionArgVarInStore\(typed_cir_module\.typeStoreConst\(\),/, allowed => {} },
    { category => 'monotype-source-fn-ret-walk', regex => qr/\blookupFunctionRetVarInStore\(typed_cir_module\.typeStoreConst\(\),/, allowed => {} },
    { category => 'monotype-source-fn-arity-walk', regex => qr/\bfunctionArgCountInStore\(typed_cir_module\.typeStoreConst\(\),/, allowed => {} },
    { category => 'monotype-source-curried-result-walk', regex => qr/\blookupCurriedFunctionResultVarInStore\(typed_cir_module\.typeStoreConst\(\),/, allowed => {} },
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

if (@violations) {
    print "Post-check architecture violations found:\n";
    print "$_\n" for @violations;
    exit 1;
}

print "Post-check architecture check passed.\n";
exit 0;
