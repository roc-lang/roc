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
    src/monotype
    src/monotype_lifted
);

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
    [qr/\bsingletonTagUnionPayloadRef\b/, "bridge-time singleton payload recovery"],
    [qr/\bsingletonZeroSizedTagUnionDiscriminant\b/, "bridge-time singleton discriminant recovery"],
    [qr/\bresolvedListElemLayoutRef\b/, "bridge-time list layout recovery"],
    [qr/\blayoutsStructurallyEqual\b/, "bridge-time structural layout recovery"],
    [qr/\blayoutRuntimeEquivalent\b/, "bridge-time runtime-equivalence recovery"],

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

if (@violations) {
    print "\nSEMANTIC AUDIT FAILED\n\n";
    print "Semantic compiler/eval/lowering code must consume explicit earlier facts.\n";
    print "Fallbacks, heuristics, reconstruction, synthesis, strengthening, and bridge-time recovery are forbidden.\n\n";
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
