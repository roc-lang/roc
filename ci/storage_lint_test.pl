use strict;
use warnings;
use Test::More;
require './ci/storage_lint.pl';

my @cases = (
    ['field backing', 'const X = struct { marks: collections.EpochSet(Id, .stamps, .internal), fn f(self: *X) void { @memset(self.marks.stamps, 0); } };', 1],
    ['constructor backing', 'var map = try collections.OptionalIndexColumn(Id).initFilled(gpa, n); @memset(map.words, 0);', 1],
    ['alias backing', 'const Col = collections.OptionalIndexColumn(Id); var map = try Col.initFilled(gpa, n); @memset(map.words[1..], 0);', 1],
    ['alias chain', 'const First = collections.EpochSet(Id, .bitset, .internal); const Second = First; var seen: Second = undefined; @memset(seen.storage, 0);', 1],
    ['alias cycle terminates', 'const A = B; const B = A; var seen: A = undefined; @memset(seen, 0);', 0],
    ['struct containing a column is not a column', 'const Analysis = struct { marks: collections.EpochSet(Id, .stamps, .internal), state: []u8 }; fn f(analysis: *Analysis) void { @memset(analysis.state, 0); }', 0],
    ['multiline', "const Col = collections.OptionalIndexColumn(\n LIR.LocalId\n); var x = try Col.initFilled(gpa, n); \@memset(\n x.words,\n 0);", 1],
    ['optional duplicate', 'const Col = collections.OptionalIndexColumn(LIR.LocalId); const x = try gpa.alloc(?LIR.LocalId, n);', 1],
    ['multiline duplicate', "const Col = collections.OptionalIndexColumn(LIR.LocalId);\nconst x = try gpa.alloc(\n ?LIR.LocalId,\n n);", 1],
    ['different payload', 'const Col = collections.OptionalIndexColumn(LIR.LocalId); const x = try gpa.alloc(?Span, n);', 0],
    ['semantic bytes', 'const bytes = try gpa.alloc(u8, n); @memset(bytes, 0);', 0],
    ['plain read-first', 'const visited = try gpa.alloc(bool, n); @memset(visited, false);', 0],
    ['column only supplies a bound', 'var map = try collections.OptionalIndexColumn(Id).initFilled(gpa, n); @memset(bytes[map.len()..], 0);', 0],
    ['column only supplies a value', 'var map = try collections.OptionalIndexColumn(Id).initFilled(gpa, n); @memset(bytes, map.len());', 0],
    ['API reset', 'var map = try collections.OptionalIndexColumn(Id).initFilled(gpa, n); map.clearAll();', 0],
    ['comment', '// var map = try collections.OptionalIndexColumn(Id).initFilled(gpa, n); @memset(map.words, 0);', 0],
    ['string', 'const text = "const Col = collections.OptionalIndexColumn(Id); gpa.alloc(?Id, n);";', 0],
    ['multiline string', "\\\\const Col = collections.OptionalIndexColumn(Id); gpa.alloc(?Id, n);\n", 0],
    ['escaped quote', 'const text = "\\" @memset(map.words, 0);"; var map = try collections.OptionalIndexColumn(Id).initFilled(gpa, n);', 0],
);
for my $case (@cases) {
    my ($name, $source, $expected) = @$case;
    my @errors = StorageLint::violations($source);
    is(scalar @errors, $expected, $name);
}
my $source = "// comment\nconst Col = collections.OptionalIndexColumn(Id);\nconst x = gpa.alloc(?Id, n);";
my @errors = StorageLint::violations($source);
is(substr($source, $errors[0][0], 6), '.alloc', 'diagnostic offset survives comment removal');
done_testing();
