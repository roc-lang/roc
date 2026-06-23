#!/usr/bin/env perl
use strict;
use warnings;
use File::Find;

my @roots = qw(
    src/check
    src/compile
    src/canonicalize
    src/eval
    src/ir
    src/lambdamono
    src/lambdasolved
    src/lir
    src/mir
    src/monotype
    src/monotype_lifted
    src/postcheck
);

my @new_postcheck_roots = qw(
    src/lambdamono
    src/lambdasolved
    src/monotype
    src/monotype_lifted
    src/postcheck
);

my @postcheck_design_docs = qw(
    design.md
);

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

my @forbidden = (
    [qr/\bResolvedMethodTarget\b/, "old resolved method target transport"],
    [qr/\bmethodCallResolvedTargets\b/, "old resolved method target transport"],
    [qr/\bmethod_call_resolved_targets\b/, "old resolved method target transport"],
    [qr/\bsetMethodCallResolvedTarget\b/, "old resolved method target transport"],
    [qr/\brecordResolvedMethodCall\b/, "old resolved method target transport"],
    [qr/\bmethod_fn_ty\b/, "ordinary methods must not carry pre-lambdamono callable targets"],
    [qr/\bselectResolvedMethodTargetIn\b/, "old lambdamono method candidate selector"],
    [qr/\bmakeExactResolvedFunctionTypeIn\b/, "old exact method callable reconstruction"],
    [qr/\brefineResolvedMethodCallable\b/, "old lambdasolved method callable refinement"],
    [qr/\bbuildResolvedMethodCallType\b/, "old pre-lambdamono method callable construction"],
    [qr/\bbuildResolvedTypeMethodCallType\b/, "old pre-lambdamono method callable construction"],
    [qr/\brequireResolvedMethodTargetSymbol\b/, "old pre-lambdamono method target lookup"],

    [qr/\blookupTopLevelBindType\b/, "late top-level source lookup reconstruction"],
    [qr/\blookupTopLevelValueSource\b/, "late top-level source lookup reconstruction"],
    [qr/\bexactConcreteResultSourceTy\b/, "late exact result source reconstruction"],
    [qr/\bfreezeFnWorld\b/, "late solved world cloning/reconstruction"],
    [qr/\bfreezeSpecializedCallWorld\b/, "late solved world cloning/reconstruction"],
    [qr/\bbuildExactRequestedFnTypeFromConstraint\b/, "exact callable request reconstruction"],

    [qr/\bconcretize[A-Za-z0-9_]*\b/, "semantic concretization/recovery helper"],
    [qr/\bstrengthen[A-Za-z0-9_]*\b/, "semantic strengthening/merge helper"],
    [qr/\bsynthesized[A-Za-z0-9_]*Type[A-Za-z0-9_]*\b/, "executable/source type synthesis from local shape"],
    [qr/\bmergedExecutable[A-Za-z0-9_]*\b/, "executable callable merge/reconstruction"],
    [qr/\bcommonErasedCaptureType[A-Za-z0-9_]*\b/, "erased capture scan/reconstruction"],
    [qr/\brefineSingletonTagSourceType[A-Za-z0-9_]*\b/, "source tag reconstruction from local syntax"],
    [qr/\bsingletonTagUnionPayloadRef\b/, "conversion-time singleton payload recovery"],
    [qr/\bsingletonZeroSizedTagUnionDiscriminant\b/, "conversion-time singleton discriminant recovery"],
    [qr/\bresolvedListElemLayoutRef\b/, "conversion-time list layout recovery"],
    [qr/\blayoutsStructurallyEqual\b/, "conversion-time structural layout recovery"],
    [qr/\blayoutRuntimeEquivalent\b/, "conversion-time runtime-equivalence recovery"],

    [qr/\be_lookup_pending\b/, "pending semantic lookup compatibility path"],
    [qr/\bexpr_pending_lookup\b/, "pending semantic lookup compatibility path"],
    [qr/\bresolvePendingLookups\b/, "pending semantic lookup compatibility path"],
    [qr/\bis_placeholder\b/, "placeholder semantic env compatibility path"],
    [qr/\.env\s*=\s*null/, "nullable semantic env compatibility path"],
    [qr/\bgetMethodIdent\b/, "method-name string fallback API"],

    [qr/\btranslateTypeVar\b/, "old comptime runtime-type recovery"],
    [qr/\bcreateTypeFromLayout\b/, "old comptime layout-to-type recovery"],
    [qr/\bflex_type_context\b/, "old comptime runtime-type recovery"],
    [qr/\bpropagateFlexMappings\b/, "old comptime runtime-type recovery"],
    [qr/\bcall_ret_rt_var\b/, "old comptime return type fallback"],
    [qr/\bexpected_rt_var\b/, "old comptime expected type fallback"],
    [qr/\bcomptime_interpreter\b/, "old semantic comptime executor"],

    [ident(qw(exact fn symbol)), "retired callable carrier"],
    [ident(qw(capture exact symbols)), "retired callable carrier"],
    [ident(qw(arg exact symbols)), "retired callable carrier"],
    [ident(qw(requested capture source tys)), "retired callable carrier"],
    [ident(qw(capture source tys)), "retired callable carrier"],
    [ident(qw(exact callable capture symbols)), "retired callable carrier"],
    [ident(qw(exact callable capture symbols by symbol)), "retired callable carrier"],
    [ident(qw(scoped exact callable capture symbols)), "retired callable carrier"],
    [camel(qw(current Exact Callable Capture Symbols)), "retired callable carrier"],
    [camel(qw(current Capture Payload From Symbols)), "retired callable carrier"],
    [camel(qw(lookup Exact Callable Capture Symbols)), "retired callable carrier"],
    [camel(qw(capture Exact Symbols From Env)), "retired callable carrier"],
    [camel(qw(exact Callable Symbol From Source Type)), "retired callable source-type reconstruction"],
    [camel(qw(exact Callable Symbol For Bound Expr)), "retired callable source-type reconstruction"],
    [camel(qw(exact Callable Capture Count)), "retired callable capture reconstruction"],
    [camel(qw(exact Callable Capture Symbols)), "retired callable capture reconstruction"],
    [qr/\bcallableF[a]ctsForSolvedArgs\b/, "retired callable arg reconstruction"],
    [camel(qw(register Scoped Exact Callable Capture Symbols)), "retired callable capture side registration"],
    [qr/\bTRACE\b/, "committed semantic investigation trace"],
    [qr/\bPlannedExec[A-Za-z0-9_]*\b/, "retired source/executable binding carrier"],
    [camel(qw(collect Planned Exec Bindings)), "retired source/executable binding carrier"],
    [camel(qw(plan Executable Type From Solved With Bindings)), "retired source/executable binding carrier"],
    [camel(qw(current Required Return Exec Ty)), "retired ambient executable return carrier"],
);

my @emit_forbidden = (
    [qr/\@import\("lambdasolved"\)/, "emitter must not import solved source types"],
    [qr/\@import\("lower_type\.zig"\)/, "emitter must not import source-to-executable type lowering"],
    [qr/\@import\("specializations\.zig"\)/, "emitter must not import specialization queues"],
    [qr/\bTypeVarId\b/, "emitter must not mention solved type variables"],
    [qr/\binternResolved\b/, "emitter must not construct executable semantic types"],
    [qr/\baddType\b/, "emitter must not construct executable semantic types"],
    [qr/\bsetType\b/, "emitter must not mutate executable semantic types"],
    [qr/\blowerExecutableTypeFromSolved[A-Za-z0-9_]*\b/, "emitter must not lower source types"],
    [qr/\bspecializations\b/, "emitter must not read specialization state"],
    [qr/\bqueue\b/, "emitter must not read specialization queues"],
);

my @lower_facade_forbidden = (
    [qr/\@import\("lambdasolved"\)/, "lower facade must not import solved source types directly"],
    [qr/\@import\("lower_type\.zig"\)/, "lower facade must not import source-to-executable type lowering"],
    [qr/\@import\("specializations\.zig"\)/, "lower facade must not import specialization queues"],
    [qr/\bTypeVarId\b/, "lower facade must not mention solved type variables"],
    [qr/\bLowerer\b/, "lower facade must not contain semantic lowering state"],
    [qr/\blowerExecutableTypeFromSolved[A-Za-z0-9_]*\b/, "lower facade must not lower source types"],
    [qr/\bdefault_ty\b/, "old optional executable result contract"],
    [qr/\bexpected_exec_ty\b/, "old optional executable result contract"],
    [qr/\bcurrent_return_exec_ty\b/, "old ambient executable return state"],
    [qr/\bSourceExecBinding\b/, "old source/executable relation binding helper"],
    [qr/\bcollectSourceExecBindings\b/, "old source/executable relation binding helper"],
    [qr/\blowerExecutableTypeFromSolvedWithBindings\b/, "old local executable type relation lowering"],
);

my @exec_plan_forbidden = (
    [qr/\bast\.Store\b/, "planner must use ExecPlan store, not final executable AST store"],
    [qr/\.result_ty\s*=\s*null/, "planner must emit explicit executable result types for defs"],
    [qr/\bdef\.result_ty\s+orelse\s+self\.output\b/, "planner must not recover def result type from emitted body"],
);

my @lambdamono_old_contract_forbidden = (
    [qr/\bpub\s+const\s+LowerType\b/, "source-to-executable type lowering must not be publicly exported"],
    [qr/\bpub\s+const\s+Specializations\b/, "specialization queues must not be publicly exported"],
    [qr/\bdefault_ty\b/, "old optional executable result contract"],
    [qr/\bexpected_exec_ty\b/, "old optional executable result contract"],
    [qr/\bcurrent_return_exec_ty\b/, "old ambient executable return state"],
    [qr/\bSourceExecBinding\b/, "old source/executable relation binding helper"],
    [qr/\bcollectSourceExecBindings\b/, "old source/executable relation binding helper"],
    [qr/\blowerExecutableTypeFromSolvedWithBindings\b/, "old local executable type relation lowering"],
    [qr/\bmergedExecutable[A-Za-z0-9_]*\b/, "executable callable merge/reconstruction"],
    [qr/\bcommonErasedCaptureType[A-Za-z0-9_]*\b/, "erased capture scan/reconstruction"],
);

my @lowering_debug_print_forbidden = (
    [qr/\bstd\.debug\.print\b/, "semantic lowering stages must not contain committed debug prints"],
);

my @postcheck_jargon_forbidden = (
    [qr/\b[A-Za-z0-9_]*[Bb]ridge[A-Za-z0-9_]*\b/, "banned vague post-check term; say the exact operation"],
    [qr/\b[A-Za-z0-9_]*[Pp]rojection[A-Za-z0-9_]*\b/, "banned vague post-check term; say field read, tag payload read, capture slot, or another exact operation"],
    [qr/\b[A-Za-z0-9_]*[Rr]eadback[A-Za-z0-9_]*\b/, "banned compile-time const term; use ConstStore wording"],
    [qr/\bread_back\b/i, "banned compile-time const term; use ConstStore wording"],
    [qr/\breification\b/i, "banned compile-time const term; use ConstStore wording"],
    [qr/\breify\b/i, "banned compile-time const term; use ConstStore wording"],
    [qr/\breified\b/i, "banned compile-time const term; use ConstStore wording"],
    [qr/\breifying\b/i, "banned compile-time const term; use ConstStore wording"],
    [qr/\bvalue[- ]graph\b/i, "banned compile-time const term; use ConstStore wording"],
    [qr/\b[A-Za-z0-9_]*ValueGraph[A-Za-z0-9_]*\b/, "banned compile-time const term; use ConstStore wording"],
    [qr/\bcompile[- ]time value store\b/i, "banned compile-time const term; use ConstStore wording"],
    [qr/\bCompileTimeValueStore\b/, "banned compile-time const term; use ConstStore wording"],
    [qr/\brepresentation[-_ ]repair\b/i, "banned mismatch-patching term; use explicit operations or invariant failure"],
    [qr/\brepair[-_ ]representation\b/i, "banned mismatch-patching term; use explicit operations or invariant failure"],
    [qr/\bRepresentationRepair\b/, "banned mismatch-patching term; use explicit operations or invariant failure"],
    [qr/\bcanonical(?:_[A-Za-z0-9_]+)?\b/i, "banned post-check term outside Canonicalization; use the exact identity, ordering, or digest term"],
    [qr/\bCanonical(?!ization\b)[A-Za-z0-9_]*\b/, "banned post-check term outside Canonicalization; use the exact identity, ordering, or digest term"],
    [qr/\b[A-Z][A-Za-z0-9_]*(?:Key|Ref)(?:[A-Z0-9_][A-Za-z0-9_]*)?\b/, "banned type-name suffix; use Id, Digest, or a concrete domain noun"],
    [qr/\bruntime[- ]image\b/i, "banned LIR image term; use LirImage"],
    [qr/\bRuntimeImage\b/, "banned LIR image term; use LirImage"],
    [qr/\bruntime_image\b/, "banned LIR image term; use LirImage"],
    [qr/\bphysical\b/i, "banned runtime-encoding term; use layout for memory shape or runtime encoding for the broader category"],
    [qr/\bartifacts?\b/i, "banned vague owner term; use CheckedModule, checked module cache, checked module data, or the exact producer/consumer"],
    [qr/\b[A-Za-z0-9_]*artifacts?[A-Za-z0-9_]*\b/i, "banned vague owner term; use CheckedModule, checked module cache, checked module data, or the exact producer/consumer"],
    [qr/\bsemantic(?:ally)?\b/i, "banned vague meaning term; use checked, source, meaning, or a precise stage-owned name"],
    [qr/\b[A-Za-z0-9_]*semantic[A-Za-z0-9_]*\b/i, "banned vague meaning term; use checked, source, meaning, or a precise stage-owned name"],
    [qr/\bexecutable\b(?!\s+(?:binary|program)\b)/i, "banned vague stage term; use the exact stage or data name"],
    [qr/\b[A-Za-z0-9_]*Executable[A-Za-z0-9_]*\b/, "banned vague stage term; use the exact stage or data name"],
    [qr/\b[A-Za-z0-9_]*executable_[A-Za-z0-9_]*\b/i, "banned vague stage term; use the exact stage or data name"],
    [qr/\b[A-Za-z0-9_]*[Oo]bligation[A-Za-z0-9_]*\b/, "banned vague post-check work item term; use checked plan, erased requirement, specialization queue entry, debug assertion, or exact deletion target"],
    [qr/\b[A-Za-z0-9_]*(?:publish|Publish)(?:ed|es|ing)?[A-Za-z0-9_]*\b/, "banned phase-output term; use output or the exact owner"],
    [qr/\b[A-Za-z0-9_]*(?:publication|Publication)[A-Za-z0-9_]*\b/, "banned phase-output term; use output or the exact owner"],
    [qr/\bfacts?\b/i, "banned vague data term; use output, data, or the exact owner"],
    [qr/\b[A-Za-z0-9_]*Fact(?:s)?[A-Za-z0-9_]*\b/, "banned vague data term; use output, data, or the exact owner"],
    [qr/\b[A-Za-z0-9_]*_facts?(?:_[A-Za-z0-9_]+)?\b/i, "banned vague data term; use output, data, or the exact owner"],
    [qr/\bfacts?_[A-Za-z0-9_]*\b/i, "banned vague data term; use output, data, or the exact owner"],
    [qr/\barena-backed typed tree\b/i, "implementation-detail phrase; use typed IR store"],
    [qr/\btyped tree stored in an arena\b/i, "implementation-detail phrase; use typed IR store"],
);

my @comment_words = qw(fallback heuristic recover reconstruct rebuild best-effort best_available best-available);

sub skip_file {
    my ($path) = @_;
    return 1 if $path !~ /\.zig$/;
    return 1 if $path =~ m{/(test|tests)/};
    return 1 if $path =~ /_test\.zig$/;
    return 1 if $path =~ m{^src/parse/};
    return 1 if $path =~ m{^src/.*/Diagnostic\.zig$};
    return 1 if $path =~ m{^src/.*/RocEmitter\.zig$};
    return 0;
}

sub comment_word_allowed {
    my ($path) = @_;
    return 1 if $path =~ m{^src/parse/};
    return 1 if $path =~ m{^src/.*/Diagnostic\.zig$};
    return 1 if $path =~ m{^src/compile/messages\.zig$};
    return 0;
}

sub is_new_postcheck_path {
    my ($path) = @_;
    for my $root (@new_postcheck_roots) {
        return 1 if $path =~ m{^\Q$root\E/};
    }
    return 0;
}

sub postcheck_jargon_allowed {
    my ($path, $line) = @_;
    if ($path eq 'design.md') {
        return 1 if $line =~ /\bbanned\b/;
        return 1 if $line =~ /The word `/;
        return 1 if $line =~ /The words `/;
        return 1 if $line =~ /The terms `/;
        return 1 if $line =~ /The suffixes `/;
        return 1 if $line =~ /pre-existing code that is scheduled for deletion/;
        return 1 if $line =~ /pre-existing code scheduled for deletion/;
    }
    return 0;
}

my @violations;

for my $root (@roots) {
    next unless -d $root;
    find({
        wanted => sub {
            my $path = $File::Find::name;
            return if skip_file($path);

            open my $fh, '<', $path or die "failed to open $path: $!";
            my $line_no = 0;
            while (my $line = <$fh>) {
                ++$line_no;
                chomp $line;

                for my $rule (@forbidden) {
                    my ($pattern, $reason) = @$rule;
                    if ($line =~ /$pattern/) {
                        push @violations, [$path, $line_no, $reason, $line];
                    }
                }

                if ($path eq 'src/lambdamono/emit.zig') {
                    for my $rule (@emit_forbidden) {
                        my ($pattern, $reason) = @$rule;
                        if ($line =~ /$pattern/) {
                            push @violations, [$path, $line_no, $reason, $line];
                        }
                    }
                }

                if ($path eq 'src/lambdamono/lower.zig') {
                    for my $rule (@lower_facade_forbidden) {
                        my ($pattern, $reason) = @$rule;
                        if ($line =~ /$pattern/) {
                            push @violations, [$path, $line_no, $reason, $line];
                        }
                    }
                }

                if ($path eq 'src/lambdamono/exec_plan.zig') {
                    for my $rule (@exec_plan_forbidden) {
                        my ($pattern, $reason) = @$rule;
                        if ($line =~ /$pattern/) {
                            push @violations, [$path, $line_no, $reason, $line];
                        }
                    }
                }

                if ($path =~ m{^src/lambdamono/}) {
                    for my $rule (@lambdamono_old_contract_forbidden) {
                        my ($pattern, $reason) = @$rule;
                        if ($line =~ /$pattern/) {
                            push @violations, [$path, $line_no, $reason, $line];
                        }
                    }
                }

                if ($path =~ m{^src/(lambdamono|lambdasolved|monotype|monotype_lifted)/}) {
                    for my $rule (@lowering_debug_print_forbidden) {
                        my ($pattern, $reason) = @$rule;
                        if ($line =~ /$pattern/) {
                            push @violations, [$path, $line_no, $reason, $line];
                        }
                    }
                }

                if (is_new_postcheck_path($path) && !postcheck_jargon_allowed($path, $line)) {
                    for my $rule (@postcheck_jargon_forbidden) {
                        my ($pattern, $reason) = @$rule;
                        if ($line =~ /$pattern/) {
                            push @violations, [$path, $line_no, $reason, $line];
                        }
                    }
                }

                if (!comment_word_allowed($path) && $line =~ /^\s*\/\//) {
                    for my $word (@comment_words) {
                        if ($line =~ /\b\Q$word\E\b/i) {
                            push @violations, [$path, $line_no, "semantic comment mentions forbidden recovery language: $word", $line];
                        }
                    }
                }
            }
            close $fh;
        },
        no_chdir => 1,
    }, $root);
}

for my $path (@postcheck_design_docs) {
    next unless -f $path;

    open my $fh, '<', $path or die "failed to open $path: $!";
    my $line_no = 0;
    while (my $line = <$fh>) {
        ++$line_no;
        chomp $line;
        next if postcheck_jargon_allowed($path, $line);

        # Vocabulary rules govern prose, not link targets.
        my $scannable = $line;
        $scannable =~ s/https?:\/\/\S+//g;

        for my $rule (@postcheck_jargon_forbidden) {
            my ($pattern, $reason) = @$rule;
            if ($scannable =~ /$pattern/) {
                push @violations, [$path, $line_no, $reason, $line];
            }
        }
    }
    close $fh;
}

if (@violations) {
    print "\nSEMANTIC AUDIT FAILED\n\n";
    print "Semantic compiler/eval/lowering code must consume explicit earlier data.\n";
    print "Fallbacks, heuristics, reconstruction, synthesis, strengthening, and conversion-time recovery are forbidden.\n\n";
    for my $violation (@violations) {
        my ($path, $line_no, $reason, $line) = @$violation;
        $line =~ s/^\s+//;
        print "$path:$line_no: $reason\n";
        print "  $line\n";
    }
    print "\nFound " . scalar(@violations) . " semantic audit violation(s).\n";
    exit 1;
}

print "Semantic audit passed.\n";
exit 0;
