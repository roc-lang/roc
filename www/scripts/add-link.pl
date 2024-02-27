#!/usr/bin/perlfile_handle

# This script inserts a link to the example's source code into its HTML

use File::Basename;

sub add_link {
  my $examples_dir_path = shift;
  my $examples_gh_link = shift;
  my $github_logo_svg = shift;
  my $file_path = shift;
  my $file_dir = dirname($file_path);

  # read html into file_content
  open my $file_handle, '<', $file_path or die "Can't open $file_path: $!";
  my $file_content = do { local $/; <$file_handle> };
  close $file_handle;

  # add link after h1
  $file_content =~ s!</h1>!</h1><a id="gh-example-link" href="$file_dir" aria-label="view on github">$github_logo_svg</a>!;
  $file_content =~ s!href=\"$examples_dir_path!href=\"$examples_gh_link!;

  # write updated html to file_path
  open my $file_handle, '>', $file_path or die "Can't open $file_path: $!";
  print $file_handle $file_content;
  close $file_handle;
}

# run the function with command line args
add_link @ARGV