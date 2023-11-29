{ lib, nix-gitignore }:
let
  # See https://nix.dev/tutorials/file-sets for an guide on how the file set api works

  fs = lib.fileset;

  fileDoesNotHaveExt = fileExts: file: (!lib.lists.any (ext: file.hasExt ext) fileExts);

  repoRoot = ../.;

  # The file set api does not currently have a way to easily remove folders dynamically
  # since the nix build does not run tests try to remove any folders with just tests
  removedTests =
    let
      dir_filter = path_str: (
        let dirName = baseNameOf path_str; in !(
          # remove any folder whos name is `tests` or starts with `test_`
          dirName == "tests"

          # TODO: while the below logic seems to work to filter out folders, 
          # cargo still cares if the path exists (even for dev) :(
          # || lib.strings.hasPrefix "test_" dirName
        )
      );
      removeTestFiler =
        path: type:
        # only do a "real" check on directory, allow everything else through
        (type == "directory" && dir_filter path)
        || type != "directory";
    in
    lib.sources.cleanSourceWith { src = repoRoot; filter = removeTestFiler; };
  fsBase = fs.fromSource removedTests;

  # fsBase = fs.fromSource repoRoot;

  # only look at files in the crates folder
  onlyCratesFolder = fs.intersection ../crates fsBase; # TODO: probably need to union back in the root cargo files

  # the above filter only has the subfolder, put some needed files from the root back in
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

  # the above filter can make the doc crate sad since it has pictures
  docsAddedBack = fs.unions [
    ../crates/docs
    removedSimpleFiles
  ];

  # ===============================
  # If you are trying to see what is ok to exclude from the "main" builds (cli/lang_server)
  # use `cargo tree` https://doc.rust-lang.org/cargo/commands/cargo-tree.html
  # 
  # Ex: `cargo tree -i roc_build` will show all deps of the `roc_build` crate
  # if only the package passed with `-i` is shown, nothing depends on it
  # ===============================

  # remove www folder from checkmate crate since its not built with nix
  removedWWW = fs.difference docsAddedBack ../crates/compiler/checkmate/www;

  # potential packages/folders that could be removed 
  # repl_* -> I don't think nix will build those

  filteredSrc = fs.toSource {
    root = repoRoot;
    # to debug you can switch to
    # fileset = fs.traceVal <file set>
    fileset = removedWWW;
  };

in
filteredSrc