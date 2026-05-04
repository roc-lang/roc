#!/usr/bin/env perl
use strict;
use warnings;

sub read_file {
    my ($path) = @_;
    open my $fh, '<', $path or die "failed to read $path: $!\n";
    local $/;
    return <$fh>;
}

sub require_match {
    my ($path, $source, $regex, $message) = @_;
    if ($source !~ /$regex/s) {
        print "$path: $message\n";
        return 1;
    }
    return 0;
}

my @violations;

my %files = map { $_ => read_file($_) } qw(
    src/lir/checked_pipeline.zig
    src/mir/debug_verify.zig
    src/mir/executable/build.zig
    src/mir/mono/specialize.zig
    src/mir/mono_row/mod.zig
    src/mir/lambda_solved/solve.zig
    src/eval/test/eval_low_level_tests.zig
);

push @violations, "src/mir/debug_verify.zig: debug verifier enabled() must be exactly Debug mode"
    unless $files{'src/mir/debug_verify.zig'} =~ /pub inline fn enabled\(\) bool \{\s*return builtin\.mode == \.Debug;\s*\}/s;

push @violations, "src/lir/checked_pipeline.zig: checked-artifact verification must be under a Debug-mode gate"
    unless $files{'src/lir/checked_pipeline.zig'} =~ /if \(builtin\.mode == \.Debug\) \{\s*switch \(target\.artifact_state\) \{\s*\.published => artifacts\.root\.artifact\.verifyPublished\(\),\s*\.checking_finalization => artifacts\.root\.artifact\.verifyReadyForCompileTimeLowering\(\),\s*\}\s*\}/s;

push @violations, "src/lir/checked_pipeline.zig: compile-time dependency verification must be Debug-only"
    unless $files{'src/lir/checked_pipeline.zig'} =~ /\.checking_finalization => if \(builtin\.mode == \.Debug\) artifacts\.root\.artifact\.verifyReadyForCompileTimeLowering\(\),/s;

push @violations, "src/lir/checked_pipeline.zig: published erased ABI verification must return immediately outside Debug"
    unless $files{'src/lir/checked_pipeline.zig'} =~ /\.published => \{\s*if \(builtin\.mode != \.Debug\) return;\s*for \(solved\.solve_sessions\.items\) \|\*session\| \{\s*session\.representation_store\.erased_fn_abis\.verifyPublished\(\);/s;

push @violations, "src/lir/checked_pipeline.zig: checking-finalization erased ABI verification must be Debug-only"
    unless $files{'src/lir/checked_pipeline.zig'} =~ /for \(solved\.solve_sessions\.items\) \|\*session\| \{\s*if \(builtin\.mode == \.Debug\) session\.representation_store\.erased_fn_abis\.verifyPublished\(\);/s;

push @violations, "src/mir/mono/specialize.zig: mono program verifier must be Debug-only"
    unless $files{'src/mir/mono/specialize.zig'} =~ /if \(\@import\("builtin"\)\.mode == \.Debug\) verifyProgram\(&program\);/s;

push @violations, "src/mir/mono_row/mod.zig: row-finalized verifier call must use the debug verifier gate"
    unless $files{'src/mir/mono_row/mod.zig'} =~ /if \(verify\.enabled\(\)\) verifyResult\(&result\);/s;

push @violations, "src/mir/mono_row/mod.zig: row-finalized verifier body must return outside Debug"
    unless $files{'src/mir/mono_row/mod.zig'} =~ /pub fn verifyResult\(result: \*const Result\) void \{\s*if \(!verify\.enabled\(\)\) return;/s;

push @violations, "src/mir/lambda_solved/solve.zig: lambda-solved sealed verifiers must be Debug-only"
    unless $files{'src/mir/lambda_solved/solve.zig'} =~ /if \(\@import\("builtin"\)\.mode == \.Debug\) \{\s*verifySealedLambdaSolvedProgram\(&program\);\s*for \(program\.solve_sessions\.items\) \|\*session\| \{\s*session\.representation_store\.verifySealed\(\);\s*\}\s*\}/s;

push @violations, "src/mir/executable/build.zig: executable MIR verifier must use the debug verifier gate"
    unless $files{'src/mir/executable/build.zig'} =~ /if \(debug\.enabled\(\)\) verifyExecutableProgram\(&program\);/s;

push @violations, "src/mir/executable/build.zig: executable MIR verifier must reject unresolved type placeholders"
    unless $files{'src/mir/executable/build.zig'} =~ /\.placeholder => debug\.invariant\(false, "executable MIR type store contains an unresolved placeholder"\),/s;

my @required_eval_fixtures = (
    'boxed lambda round trip: direct proc-value capture transform',
    'boxed lambda round trip: proc value captures erased callable',
    'boxed lambda round trip: branch join packs finite closure into erased result',
    'boxed lambda round trip: erased function argument transform',
    'boxed lambda round trip: erased function result transform',
    'boxed lambda round trip: erased record callable field transform',
    'boxed lambda round trip: erased list callable element transform',
    'boxed lambda round trip: erased tag payload callable transform',
    'boxed lambda round trip: nested box does not authorize unrelated erasure',
    'non-boxed function containers stay finite callable values',
);

for my $fixture (@required_eval_fixtures) {
    push @violations, "src/eval/test/eval_low_level_tests.zig: missing MIR cutover eval fixture: $fixture"
        unless index($files{'src/eval/test/eval_low_level_tests.zig'}, $fixture) >= 0;
}

if (@violations) {
    print "MIR cutover contract violations found:\n";
    print "$_\n" for @violations;
    exit 1;
}

print "MIR cutover contract check passed.\n";
exit 0;
