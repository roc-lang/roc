#!/usr/bin/perl

$num_args = $#ARGV + 1;

if ($num_args != 1) {
    die "\nUsage: import.pl path-to-roc_builtins_bitcode-hashgoeshere.bc\n";
}

my $filename = $ARGV[0];
my $before_bitcode = "// GENERATED FILE - NEVER EDIT BY HAND!\n//\n// See compiler/builtins/bitcode/README.md for how to generate.\n\npub const BUILTINS_BITCODE: &[u8] = &[\n    ";
my $after_bitcode = "\n];";

# Get a filehandle to the raw binary data in the file
open(my $fh, '<:raw', $filename)
    or die "Could not open file '$filename'\n\n$!";

my $bitcode = '';

while (1) {
    # Read 1 byte
    my $success = read $fh, my $byte, 1;

    if (not defined $success) {
        # Explode on error.
        die $!
    } elsif (not $success) {
        # Exit the loop if no bytes were read.
        last;
    } else {
        if (length($bitcode) > 0) {
            $bitcode .= ', ';
        }

        # Print the numeric representation of the byte
        $bitcode .= ord($byte);
    }
}
close $fh;

print "$before_bitcode$bitcode$after_bitcode\n";
