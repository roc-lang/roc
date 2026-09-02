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
    # Content-based nominal identity: module/type name TEXT must never decide
    # identity downstream of import resolution. These deleted text-matching
    # APIs must stay gone; identity crosses stores only as 32-byte content
    # hashes (rebase via lookupModuleIdentity/internModuleIdentity).
    { category => 'text-identity-module-view-match', regex => qr/\bmoduleViewNameMatches\b/, allowed => {} },
    { category => 'text-identity-owner-remap', regex => qr/\bmethodOwnerInImportedNames\b/, allowed => {} },
    { category => 'text-identity-owner-env-dedup', regex => qr/\bmoduleEnvNamesMatch\b/, allowed => {} },
    { category => 'text-identity-owner-env-match', regex => qr/\bownerModuleEnvNameMatches\b/, allowed => {} },
    { category => 'text-identity-owner-env-map', regex => qr/\bbuildOwnerModuleEnvMap\b/, allowed => {} },
    { category => 'text-identity-owner-env-map', regex => qr/\bputOwnerModuleEnvNames\b/, allowed => {} },
    { category => 'text-identity-imported-view-match', regex => qr/\bimportedViewModuleNameMatches\b/, allowed => {} },
    { category => 'text-identity-public-api-dep', regex => qr/\bpublicApiDependencyViewByModuleName\b/, allowed => {} },
    { category => 'text-identity-public-api-dep', regex => qr/\bisSelfPublicApiModuleName\b/, allowed => {} },
    { category => 'text-identity-glue-def-probe', regex => qr/\bfindTopLevelDefByName\b/, allowed => {} },
    { category => 'text-identity-artifact-env-match', regex => qr/\bmoduleEnvNameMatches\b/, allowed => {} },
    { category => 'body-const-global-type-owner', regex => qr/\bconstBuilder\b/, allowed => {} },
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

        # `zig_code_line` walks the line one character at a time, so asking for
        # it inside the rule loop repeats that walk for every rule the line is
        # checked against. One line's stripped form is the same for all of
        # them, so compute it at most once and hand the same string to each.
        my $code_subject;

        for my $rule (@RULES) {
            next if $rule->{allowed}{$rel};
            # Architecture rules constrain Zig tokens, not prose or fixture
            # strings. A rule must opt in explicitly if raw source text matters.
            my $subject;
            if ($rule->{raw_text}) {
                $subject = $line;
            } else {
                $code_subject = zig_code_line($line) unless defined $code_subject;
                $subject = $code_subject;
            }
            if ($subject =~ $rule->{regex}) {
                push @violations, "$rel:$line_no: $rule->{category}: $line";
            }
        }
    }

    close $fh or die "failed to close $rel: $!\n";
}

sub zig_code_line {
    my ($line) = @_;

    # Zig multiline string contents start with `\\` after indentation and run
    # through the end of that source line.
    return '' if $line =~ /^\s*\\\\/;

    my $code = '';
    my $quote;
    my $quoted_identifier = 0;
    my $escaped = 0;
    for (my $index = 0; $index < length($line); ++$index) {
        my $char = substr($line, $index, 1);
        if (defined $quote) {
            if ($escaped) {
                $escaped = 0;
                next;
            }
            if ($char eq '\\') {
                $escaped = 1;
                next;
            }
            if ($char eq $quote) {
                undef $quote;
                $quoted_identifier = 0;
            } elsif ($quoted_identifier && $char ne '{' && $char ne '}') {
                $code .= $char;
            }
            next;
        }

        if ($char eq '"' || $char eq "'") {
            $quote = $char;
            $quoted_identifier = $char eq '"' && $index > 0 && substr($line, $index - 1, 1) eq '@';
            next;
        }
        if ($char eq '/' && substr($line, $index + 1, 1) eq '/') {
            last;
        }
        $code .= $char;
    }

    return $code;
}

sub zig_code_line_with_strings {
    my ($line) = @_;

    return '' if $line =~ /^\s*\\\\/;

    my $code = '';
    my $quote;
    my $escaped = 0;
    for (my $index = 0; $index < length($line); ++$index) {
        my $char = substr($line, $index, 1);
        if (defined $quote) {
            $code .= $char;
            if ($escaped) {
                $escaped = 0;
                next;
            }
            if ($char eq '\\') {
                $escaped = 1;
                next;
            }
            if ($char eq $quote) {
                undef $quote;
            }
            next;
        }

        if ($char eq '"' || $char eq "'") {
            $quote = $char;
            $code .= $char;
            next;
        }
        if ($char eq '/' && substr($line, $index + 1, 1) eq '/') {
            last;
        }
        $code .= $char;
    }

    return $code;
}

sub brace_delta {
    my ($code) = @_;
    my $opens = ($code =~ tr/{/{/);
    my $closes = ($code =~ tr/}/}/);
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
    my %allowed_global_type_ingress = map { $_ => 1 } qw(
        lowerType
        lowerTypeFromView
        primitiveType
    );
    my %allowed_global_type_access = map { $_ => 1 } qw(
        importProgramType
        commitGraphType
        programFnSourceTypeNode
    );
    my %allowed_global_name_access = map { $_ => 1 } qw(
        importProgramType
        commitGraphType
        lookupMethodTargetByName
    );
    my %allowed_graph_type_egress = map { $_ => 1 } qw(
        completeDeferredIteratorResult
    );

    my $direct_output = qr/self\.builder\.program\.(?:addExpr|addPat|addLocal|addLocalWithBinder|addFn|addExprSpan|addPatSpan|addTypedLocalSpan|addStmt|addStmtSpan|addFieldExprSpan|addRecordDestructSpan|addBranchSpan|addIfBranchSpan|addStrPatternStepSpan|addStringLiteral|addStringView|addComptimeSite|addStaticDataValue|addStaticDataValueAssumeCapacity|ensureStaticDataValueCapacity|static_data_values|defs\.append|defs\.items|exprs\.items|pats\.items|locals\.items)\b/;
    my $global_type_access = qr/(?:\.builder\.program\.(?:fnSource|types)|self\.builder\.(?:constBoxPayloadType|constListElemType|constRecordFields|errorRowIsIncludedIn|functionShape|namedBackingType|nominalConstructionLayer|nominalExprBackingType|optionalFieldSlot|optionalSlotInfo|recordField|recordFieldByTextOptional|recordFieldType|recordFieldsSpan|shapeContent|singleTypeArg|specializationTypeDigest|tagByName|tagPayloadTypes|tagUnionTags|tupleItemTypes|typeHasBuiltinOwner|typeIsProvenUninhabited))\b/;
    my $global_type_ingress = qr/self\.builder\.(?:lowerType|primitiveType)\b/;
    my $global_type_cache = qr/\.builder\.(?:type_cache|parse_result_ok_types|generated_try_types|uninhabited_type_cache)\.(?:get|getPtr|getOrPut|put|remove)\b/;
    my $global_name_access = qr/(?:self\.builder\.program\.names|self\.builder\.(?:moduleIdentity|recordFieldName|tagName|typeName|typeDef|lookupMethodTarget|lookupMethodTargetByName))\b/;
    my $graph_type_egress = qr/self\.commitGraphType\b/;
    my $coordinator_static_data = qr/self\.builder\.commitStaticDataValue\b/;

    my $in_body_context = 0;
    my $body_depth = 0;
    my $current_fn;
    my $fn_started = 0;
    my $fn_depth = 0;
    my $line_no = 0;

    while (my $line = <$fh>) {
        ++$line_no;
        chomp $line;
        my $code = zig_code_line($line);

        if (!$in_body_context) {
            if ($code =~ /^\s*const\s+BodyContext\s*=\s*struct\s*\{/) {
                $in_body_context = 1;
                $body_depth = brace_delta($code);
            }
            next;
        }

        if (!defined $current_fn && $code =~ /^\s+(?:pub\s+)?fn\s+([A-Za-z0-9_]+)\b/) {
            $current_fn = $1;
            $fn_started = 0;
            $fn_depth = 0;
        }

        if ($code =~ $direct_output && !$allowed_fn{$current_fn // ''}) {
            push @violations, "$rel:$line_no: body-context-final-output: $line";
        }
        if ($code =~ $global_type_access && !$allowed_global_type_access{$current_fn // ''}) {
            push @violations, "$rel:$line_no: body-context-global-type-store: $line";
        }
        if ($code =~ $global_type_ingress && !$allowed_global_type_ingress{$current_fn // ''}) {
            push @violations, "$rel:$line_no: body-context-global-type-ingress: $line";
        }
        if ($code =~ $global_type_cache) {
            push @violations, "$rel:$line_no: body-context-builder-global-type-cache: $line";
        }
        if ($code =~ $global_name_access && !$allowed_global_name_access{$current_fn // ''}) {
            push @violations, "$rel:$line_no: body-context-global-name-store: $line";
        }
        if ($code =~ $graph_type_egress && !$allowed_graph_type_egress{$current_fn // ''}) {
            push @violations, "$rel:$line_no: body-context-global-type-egress: $line";
        }
        if ($code =~ $coordinator_static_data) {
            push @violations, "$rel:$line_no: body-context-coordinator-static-data: $line";
        }

        my $delta = brace_delta($code);
        $body_depth += $delta;

        if (defined $current_fn) {
            if (!$fn_started && $code =~ /\{/) {
                $fn_started = 1;
            }
            $fn_depth += $delta if $fn_started;
            if ($fn_started && $fn_depth <= 0) {
                undef $current_fn;
                $fn_started = 0;
                $fn_depth = 0;
            }
        }

        # Helper strategy structs declared after BodyContext receive
        # `self: *BodyContext` and share the same ownership restrictions. Keep
        # scanning the rest of the module so they cannot bypass this boundary.
        $body_depth = 0 if $body_depth <= 0;
    }

    close $fh or die "failed to close $rel: $!\n";
}

check_body_context_output_access();

sub check_body_context_bridge_type_access {
    my $rel = 'src/postcheck/monotype/lower.zig';
    my $path = File::Spec->catfile($ROOT, $rel);
    open my $fh, '<', $path or die "failed to read $rel: $!\n";

    my %bridge_fn = map { $_ => 1 } qw(
        lowerDraftNestedFromContext
        lowerDraftTemplateFromContext
    );
    my $global_type_access = qr/self\.(?:program\.types|specializationTypeDigest)\b/;
    my $current_fn;
    my $fn_started = 0;
    my $fn_depth = 0;
    my $line_no = 0;

    while (my $line = <$fh>) {
        ++$line_no;
        chomp $line;
        my $code = zig_code_line($line);

        if (!defined $current_fn && $code =~ /^\s+fn\s+([A-Za-z0-9_]+)\b/) {
            $current_fn = $1;
            $fn_started = 0;
            $fn_depth = 0;
        }

        if ($bridge_fn{$current_fn // ''} && $code =~ $global_type_access) {
            push @violations, "$rel:$line_no: body-context-bridge-global-type-store: $line";
        }

        my $delta = brace_delta($code);
        if (defined $current_fn) {
            if (!$fn_started && $code =~ /\{/) {
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

check_body_context_bridge_type_access();

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
        my $code = zig_code_line($line);

        if (!$in_test && $code =~ /^\s*test\s+/) {
            $in_test = 1;
            $test_started = 0;
            $test_depth = 0;
        }

        if (!$in_test && !defined $current_fn && $code =~ /^\s+fn\s+([A-Za-z0-9_]+)\b/) {
            $current_fn = $1;
            $fn_started = 0;
            $fn_depth = 0;
        }

        # `sealActiveBodyDraft` composes the synchronous path, while
        # Finalized graph-backed bodies and sealed graph-free shards each own
        # one ordered draft commit suffix.
        my $owns_body_draft_commit =
            ($current_fn // '') eq 'sealActiveBodyDraft' ||
            ($current_fn // '') eq 'commitFinalizedBodyDraft' ||
            ($current_fn // '') eq 'commitSealedBodyDraft';
        if (!$in_test && !$owns_body_draft_commit) {
            if ($code =~ /\b[A-Za-z_][A-Za-z0-9_]*\.sealCoreIntoProgram\(/) {
                push @violations, "$rel:$line_no: active-body-draft-seal-bypass: $line";
            }
            if ($code =~ /\b[A-Za-z_][A-Za-z0-9_]*\.markNestedReady\(/) {
                push @violations, "$rel:$line_no: active-body-draft-seal-bypass: $line";
            }
            if ($code =~ /\b[A-Za-z_][A-Za-z0-9_]*\.seal\(self,\s*graph,\s*&sealer/) {
                push @violations, "$rel:$line_no: active-body-draft-seal-bypass: $line";
            }
            if ($code =~ /\bBodyDraftStore\.finalIdOffsets\(self\.program\)/) {
                push @violations, "$rel:$line_no: active-body-draft-seal-bypass: $line";
            }
        }
        if (!$in_test && ($current_fn // '') ne 'activeTypeFromNode') {
            if ($code =~ /\bactiveTypeViewForNode\(/) {
                push @violations, "$rel:$line_no: active-graph-view-bypass: $line";
            }
        }

        my $delta = brace_delta($code);

        if ($in_test) {
            if (!$test_started && $code =~ /\{/) {
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
            if (!$fn_started && $code =~ /\{/) {
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

sub check_iterator_lowering_uses_explicit_ids {
    my $rel = 'src/postcheck/monotype/lower.zig';
    my $path = File::Spec->catfile($ROOT, $rel);
    open my $fh, '<', $path or die "failed to read $rel: $!\n";

    my %bodies;
    my $current_fn;
    my $fn_started = 0;
    my $fn_depth = 0;
    my $line_no = 0;

    while (my $line = <$fh>) {
        ++$line_no;
        chomp $line;
        my $code = zig_code_line($line);
        my $code_with_strings = zig_code_line_with_strings($line);

        if (!defined $current_fn && $code =~ /^\s+(?:pub\s+)?fn\s+([A-Za-z0-9_]+)\b/) {
            $current_fn = $1;
            $fn_started = 0;
            $fn_depth = 0;
        }

        if (defined $current_fn) {
            push @{$bodies{$current_fn}}, [$line_no, $line, $code_with_strings];
            my $delta = brace_delta($code);
            if (!$fn_started && $code =~ /\{/) {
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

    # Match reconstruction of the iterator protocol's semantic roles directly,
    # regardless of the containing helper's spelling. `Done` is shared by
    # other compiler protocols, so only a text lookup against a step-result
    # type is an iterator-shape recovery.
    my $field_role = qr/(?:len_if_known|step|item|rest)/;
    my $tag_role = qr/(?:Known|Unknown|One|Skip)/;
    for my $fn (keys %bodies) {
        for my $entry (@{$bodies{$fn}}) {
            my ($source_line, $line, $code) = @$entry;
            my $recovers_by_text =
                $code =~ /\brecordFieldByText(?:Optional)?\s*\([^\)]*"$field_role"/ ||
                $code =~ /\bmonoTagByText(?:Optional)?\s*\([^\)]*"$tag_role"/ ||
                $code =~ /\bmonoTagByText(?:Optional)?\s*\([^,]*step[^,]*,\s*"Done"/ ||
                $code =~ /\b(?:recordFieldLabelTextEql|tagLabelTextEql)\s*\([^\)]*"(?:$field_role|$tag_role)"/ ||
                $code =~ /\bIdent\.textEql\s*\([^\)]*"(?:$field_role|$tag_role)"/;
            if ($recovers_by_text) {
                push @violations, "$rel:$source_line: iterator-text-shape-recognition: $line";
            }
        }
    }
}

check_iterator_lowering_uses_explicit_ids();

if (@violations) {
    print "Post-check architecture violations found:\n";
    print "$_\n" for @violations;
    exit 1;
}

print "Post-check architecture check passed.\n";
exit 0;
