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
let
  packageDependencies = pkgs.stdenv.mkDerivation {
    inherit src outputHash;
    name = "roc-dependencies";
    nativeBuildInputs = with pkgs; [ gnutar brotli ripgrep wget cacert ];

    buildPhase = ''
      declare -A visitedUrls

      # function that recursively prefetches roc dependencies
      # so they're available during roc build stage
      function prefetch () {
        local searchPath=$1

        local dependenciesRegexp='https://[^"]*tar.br|https://[^"]*tar.gz'
        local getDependenciesCommand="rg -o '$dependenciesRegexp' -IN $searchPath"
        local depsUrlsList=$(eval "$getDependenciesCommand")

        if [ -z "$depsUrlsList" ]; then
          echo "Executed: $getDependenciesCommand"
          echo "No URLs found in $searchPath"
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
            set -e
            if ! (wget -P "$outputPackagePath" "$url" 2>/tmp/wget_error); then
              echo "WARNING: Failed to download $url: $(cat /tmp/wget_error)"
              continue 
            fi
            set -e

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
            prefetch "$outputPackagePath"
          fi
        done
      }

      prefetch ${src}

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

