#!/usr/bin/env perl
use strict;
use warnings;

my $path = "src/backend/wasm/WasmCodeGen.zig";
open my $fh, "<", $path or die "cannot open $path: $!\n";

my @violations;
my $line_number = 0;
while (my $line = <$fh>) {
    $line_number += 1;
    if ($line =~ /\b\w+_import\b[^\n;]*\borelse\s+unreachable\b/) {
        push @violations, "$path:$line_number: builtin host import unwrapped outside emitBuiltinCall\n";
    }
    if ($line =~ /emitCall\s*\(\s*self\.\w+_import\b/) {
        push @violations, "$path:$line_number: builtin import called directly instead of emitBuiltinCall\n";
    }
    if ($line =~ /const\s+\w*import\w*\s*=\s*self\.\w+_import\b/) {
        push @violations, "$path:$line_number: builtin import copied to a raw call index\n";
    }
}
close $fh;

if (@violations) {
    print STDERR "WASM builtin routing architecture violations:\n";
    print STDERR @violations;
    print STDERR "Every Roc builtin call must go through emitBuiltinCall with an explicit BuiltinKind.\n";
    exit 1;
}

exit 0;
