{ pkgs, ... }:

let
  version = "1.86.1";
in
pkgs.stdenv.mkDerivation rec {
  pname = "vscode-js-debug";
  inherit version;

  src = pkgs.fetchzip {
    url = "https://github.com/microsoft/vscode-js-debug/releases/download/v${version}/js-debug-dap-v${version}.tar.gz";
    sha256 = "sha256-POPj3jPxSuAz91lgH474R1Sfo9U1qX5H1RHdT3+DXsA=";
  };

  buildInputs = [ pkgs.nodejs ];
  phases = [ "unpackPhase" "installPhase" "postInstall" ];
  
  installPhase = ''
    mkdir -p $out/lib
    cp -r $src/* $out/lib
  '';

  postInstall = ''
      exe=$out/bin/dapDebugServer
      mkdir -p $out/bin
      cat >$exe <<EOF
      #!${pkgs.runtimeShell}
      exec -a dapDebugServer ${pkgs.nodejs}/bin/node $out/lib/src/dapDebugServer.js \$@
      EOF
      chmod a+x $exe
    '';
}

