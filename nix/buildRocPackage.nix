{ pkgs, roc-cli, name, entryPoint, src, outputHash, ... }:
let
  packageDependencies = pkgs.stdenv.mkDerivation {
    inherit src;
    name = "roc-dependencies";
    nativeBuildInputs = with pkgs; [ gnutar brotli ripgrep wget ];

    buildPhase = ''
      list=$(rg -o 'https://github.com[^"]*' ${entryPoint})
      for url in $list; do
        path=$(echo $url | awk -F'github.com/|/[^/]*$' '{print $2}')
        packagePath=$out/roc/packages/github.com/$path
        mkdir -p $packagePath
        wget -P $packagePath $url --no-check-certificate
        cd $packagePath
        brotli -d *.tar.br
        tar -xf *.tar --one-top-level
        rm *.tar
      done
    '';

    outputHashMode = "recursive";
    outputHashAlgo = "sha256";
    outputHash = outputHash;
  };
in pkgs.stdenv.mkDerivation {
  inherit name src;
  nativeBuildInputs = [ roc-cli ];
  buildPhase = ''
    export XDG_CACHE_HOME=${packageDependencies}
    roc build ${entryPoint} --output ${name} --optimize --linker=legacy

    mkdir -p $out/bin
    mv ${name} $out/bin/${name}
  '';
}

