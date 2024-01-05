#!/usr/bin/perl

use File::Basename;

sub add_link {
  my $examples_dir_path = shift;
  my $examples_gh_link = shift;
  my $github_logo_svg = shift;
  my $file_path = shift;
  my $file_dir = dirname($file_path);

  open my $fh, '<', $file_path or die "Can't open $file_path: $!";
  my $content = do { local $/; <$fh> };
  close $fh;

  $content =~ s!</h1>!</h1><a id="gh-example-link" href="$file_dir" aria-label="view on github">$github_logo_svg</a>!;
  $content =~ s!href=\"$examples_dir_path!href=\"$examples_gh_link!;

  open my $fh, '>', $file_path or die "Can't open $file_path: $!";
  print $fh $content;
  close $fh;
}

add_link @ARGV