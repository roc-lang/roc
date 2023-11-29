{ lib, nix-gitignore }:
let
  # See https://nix.dev/tutorials/file-sets for an guide on how the file set api works

  fs = lib.fileset;

  fileDoesNotHaveExt = fileExts: file: (!lib.lists.any (ext: file.hasExt ext) fileExts);

  repoRoot = ../.;


  # "old" way of filtering files that fileset does not support yet
  # remove anything ignored by git
  # baseSrc = nix-gitignore.gitignoreSource [ ] repoRoot;

  fsBase = fs.fromSource repoRoot;


  # only look at files in the crates folder
  onlyCratesFolder = fs.intersection ../crates fsBase; # TODO: probably need to union back in the root cargo files

  # the above filter only has the subfolder, put the cargo.toml and lock back
  includeCargoRootFiles = fs.unions [
    ../Cargo.toml
    ../Cargo.lock
    ../version.txt
    onlyCratesFolder

  ];

  # Remove any "simple" files like markdown/pictures since they probably wont be used in the actual code
  removedSimpleFiles =
    let
      extensionsToRemove = [ "md" "svg" "png" ];
    in
    fs.intersection
      includeCargoRootFiles
      (fs.fileFilter (fileDoesNotHaveExt extensionsToRemove) repoRoot);

  # the above filter can make the doc crate sad since it deals with pictures
  docsAddedBack = fs.unions [
    ../crates/docs
    removedSimpleFiles
  ];


  #
  # If you are trying to see what is ok to exclude from the "main" builds (cli/lang_server)
  # use `cargo tree` https://doc.rust-lang.org/cargo/commands/cargo-tree.html
  # 
  # Ex: `cargo tree -i roc_build` will show all deps of the `roc_build` crate
  # if only the package passed with `-i` is shown, nothing depends on it

in
fs.toSource {
  root = repoRoot;
  # to debug you can switch to
  # fileset = fs.traceVal <file set>
  fileset = docsAddedBack;
}
