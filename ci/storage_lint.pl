package StorageLint;
use strict;
use warnings;

# This source-level gate protects the storage types already selected by owners.
# It is not a Zig type checker: it resolves file-local aliases and declarations,
# not imported function return types. Collection implementations own their fills.
my $ident = qr/[A-Za-z_][A-Za-z_0-9]*/;
my $path = qr/$ident(?:\s*\.\s*$ident)*/;
my $storage = qr/(?:OptionalIndexColumn|ZeroSentinelIndexColumn|EpochSet|EpochColumn|WriteFirstSlice|ReservedList|ReservedColumns|SparseSlice|CsrGroups|SortedGroups)/;

sub code_only {
    my ($source) = @_;
    # Preserve offsets and newlines for diagnostics. Zig has no block comments;
    # quoted identifiers are kept because they are identifiers, not strings.
    $source =~ s{
        (\@"(?:\\.|[^"\\])*")
        | (//[^\n]*|\\\\[^\n]*|"(?:\\.|[^"\\])*"|'(?:\\.|[^'\\])*')
    }{
        defined($1) ? $1 : ($2 =~ s/[^\n]/ /gr)
    }gex;
    return $source;
}

sub canonical {
    my ($text) = @_;
    $text =~ s/\s+//g;
    return $text;
}

sub violations {
    my ($source) = @_;
    my $code = code_only($source);
    my (%aliases, %owners, %compact_indices);
    my @errors;

    # Resolve aliases to storage constructors, including aliases of aliases.
    while ($code =~ /\bconst\s+($ident)\s*=\s*([^;]+);/g) {
        $aliases{$1} = canonical($2);
    }
    my $is_storage = sub {
        my ($type) = @_;
        $type = canonical($type);
        $type =~ s/^\*(?:const)?//;
        my %seen;
        while (!$seen{$type}++ && exists $aliases{$type}) {
            $type = $aliases{$type};
        }
        return $type =~ /^(?:$ident\.)*$storage\(/;
    };

    while ($code =~ /(?:^|\W)(?:$path\.)?(?:OptionalIndexColumn|ZeroSentinelIndexColumn)\s*\(\s*($path)\s*\)/g) {
        $compact_indices{canonical($1)} = 1;
    }

    # Explicit field/parameter/local types and inferred constructor calls.
    while ($code =~ /\b($ident)\s*:\s*(\*?\s*(?:const\s+)?$path(?:\s*\([^;{}\n]*?\))?(?:\.View)?)/g) {
        $owners{$1} = 1 if $is_storage->($2);
    }
    while ($code =~ /\b(?:var|const)\s+($ident)\s*=\s*(?:try\s+)?($path(?:\s*\([^;{}\n]*?\))?)\s*\.\s*(?:init\w*|fromIndices)\s*\(/g) {
        $owners{$1} = 1 if $is_storage->($2);
    }

    while ($code =~ /\@memset\s*\(\s*([^,;]+),/g) {
        my ($target, $offset) = ($1, $-[0]);
        # A projection or slice of owned backing still belongs to the owner.
        # An owner mentioned only in a slice bound is not the filled receiver.
        my ($receiver) = $target =~ /^\s*&?\s*($path)/;
        next unless defined $receiver;
        my @parts = $receiver =~ /($ident)/g;
        if (grep { $owners{$_} } @parts) {
            push @errors, [$offset, "fill through lifecycle storage; use its initialization/reset API"];
        }
    }
    while ($code =~ /\.\s*alloc\s*\(\s*\?\s*($path)\s*,/g) {
        my ($type, $offset) = (canonical($1), $-[0]);
        if ($compact_indices{$type}) {
            push @errors, [$offset, "optional $type allocation duplicates an existing compact index column"];
        }
    }
    return @errors;
}

1;
