{ pkgs, roc-cli, name, entryPoint, src, outputHash, linker ? "", optimize ? true
, ... }:
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
        local list=$(rg -o 'https://github.com[^"]*tar.br|https://github.com[^"]*tar.gz' -IN $1)

        if [ -z "$list" ]; then
          echo "No URLs found in $1"
        fi

        for url in $list; do
          if [[ -n "''${visitedUrls["$url"]^^}" ]]; then
            echo "Skipping already visited URL: $url"
          else
            echo "Prefetching $url"
            visitedUrls["$url"]=1

            local path=$(echo $url | awk -F'github.com/|/[^/]*$' '{print $2}')
            local packagePath="$out/roc/packages/github.com/$path"
            echo "Package path: $packagePath"

            mkdir -p "$packagePath"

            # Download dependency
            set -e
            if ! (wget -P "$packagePath" "$url" 2>/tmp/wget_error); then
              echo "WARNING: Failed to download $url: $(cat /tmp/wget_error)"
              continue 
            fi
            set -e

            # Unpack dependency
            if [[ $url == *.br ]]; then
              brotli -d "$packagePath"/*.tar.br
              tar -xf "$packagePath"/*.tar --one-top-level -C $packagePath
            elif [[ $url == *.gz ]]; then
              tar -xzf "$packagePath"/*.tar.gz --one-top-level -C $packagePath
            fi

            # Delete temporary files
            rm "$packagePath"/*tar*

            # Recursively fetch dependencies of dependencies
            prefetch "$packagePath"
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
in pkgs.stdenv.mkDerivation {
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

