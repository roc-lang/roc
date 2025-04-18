
{ pkgs
, roc-cli
, name
, entryPoint
, src
, outputHash
, linker ? ""
, optimize ? true
, ...
}:
# see checks.canBuildRocPackage in /flake.nix for example usage
let
  packageDependencies = pkgs.stdenv.mkDerivation {
    inherit src outputHash;
    name = "roc-dependencies";
    nativeBuildInputs = with pkgs; [ gnutar brotli ripgrep wget cacert ];

    buildPhase = ''
      set -x

      declare -A visitedUrls

      # function that recursively prefetches roc dependencies
      # so they're available during roc build stage
      function prefetch () {
        local searchPath=$1
        local skipApp=$2 # to skip any code gen app files in dependencies

        echo "Searching for deps in $searchPath"
        
        # Set default value for skipApp if not provided
        if [ -z "$skipApp" ]; then
          skipApp=false
        fi

        local dependenciesRegexp='https://[^"]*tar.br|https://[^"]*tar.gz'
        
        # If skipApp is true, exclude files containing app declarations
        if [ "$skipApp" = true ]; then
          # Find files containing app declarations
          local appFiles=$(rg -l '^\s*app\s*\[' -IN $searchPath)
          echo "appFiles $appFiles"
          
          # If app files were found, exclude them from search
          if [ -n "$appFiles" ]; then
            local filesWithUrls=$(rg -l "$dependenciesRegexp" -IN  $searchPath)

            for appFile in $appFiles; do
              local appFullPath=$(realpath "$appFile")
              # Remove appFullPath from depsUrlsList
              filesWithUrls=$(echo "$filesWithUrls" | grep -vxF "$appFullPath" || true)
            done

            if [ -n "$filesWithUrls" ]; then
              local depsUrlsList=$(rg -o "$dependenciesRegexp" -IN  $filesWithUrls)
            else
              return 0
            fi
          else
            local depsUrlsList=$(rg -o "$dependenciesRegexp" -IN  $searchPath)
          fi
        else
          local depsUrlsList=$(rg -o "$dependenciesRegexp" -IN  $searchPath)
        fi

        depsUrlsList=$(echo "$depsUrlsList" | tr ' ' '\n' | sort | uniq | tr '\n' ' ' | sed 's/ $//')
        echo "depsUrlsList: $depsUrlsList"

        if [ -z "$depsUrlsList" ]; then
          echo "No dependency URLs need to be downloaded."
          return 0
        fi

        for url in $depsUrlsList; do
          if [[ -n "''${visitedUrls["$url"]^^}" ]]; then
            echo "Skipping already visited URL: $url"
          else
            echo "Prefetching $url"
            visitedUrls["$url"]=1

            local domain=$(echo $url | awk -F '/' '{print $3}')

            local packagePath=$(echo $url | awk -F "$domain/|/[^/]*$" '{print $2}')
            local outputPackagePath="$out/roc/packages/$domain/$packagePath"
            echo "Package path: $outputPackagePath"

            mkdir -p "$outputPackagePath"

            # Download dependency
            if ! (wget -P "$outputPackagePath" "$url" 2>/tmp/wget_error); then
              echo "WARNING: Failed to download $url: $(cat /tmp/wget_error)"
              exit 1
            fi

            # Unpack dependency
            if [[ $url == *.br ]]; then
              brotli -d "$outputPackagePath"/*.tar.br
              tar -xf "$outputPackagePath"/*.tar --one-top-level -C $outputPackagePath
            elif [[ $url == *.gz ]]; then
              tar -xzf "$outputPackagePath"/*.tar.gz --one-top-level -C $outputPackagePath
            fi

            # Delete temporary files
            rm "$outputPackagePath"/*tar*

            # Recursively fetch dependencies of dependencies
            # Pass the skipApp parameter to recursive calls
            prefetch "$outputPackagePath" true
          fi
        done
      }

      prefetch ${src} false

      if [ -d "$out/roc/packages" ]; then
        echo "Successfully prefetched packages:"
        find "$out/roc/packages" -type d -mindepth 3 -maxdepth 3 | sort
      else
        echo "WARNING: No packages were prefetched. This might indicate a problem."
      fi
    '';

    outputHashMode = "recursive";
    outputHashAlgo = "sha256";
  };
in
pkgs.stdenv.mkDerivation {
  inherit name src;
  nativeBuildInputs = [ roc-cli ];
  XDG_CACHE_HOME = packageDependencies;

  buildPhase = ''
    roc build ${entryPoint} --output ${name} \
    ${if optimize == true then "--optimize" else ""} \
    ${if linker != "" then "--linker=${linker}" else ""}

    mkdir -p $out/bin
    mv ${name} $out/bin/${name}
  '';
}
