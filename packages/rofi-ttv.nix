{ pkgs, ... }:

let
  version = "1.0";
in
pkgs.stdenv.mkDerivation rec {
  pname = "rofi-ttv-wayland";
  inherit version;

  src = builtins.fetchGit {
    url = "https://github.com/loiccoyle/rofi-ttv.git";
    rev = "ec6c1d1b42926aa3fe7f9f40ccdc37e911fa500e";
  };

  buildInputs = [
    pkgs.curl
    pkgs.jq
    pkgs.rofi-wayland
    pkgs.youtube-dl
    pkgs.mpv
    pkgs.nmap
    pkgs.netcat
  ];
  
  phases = [ "installPhase" "postInstall" ]; 
  
  installPhase = ''
    mkdir -p $out/bin
    cp -r $src/rofi-ttv* $out/bin/rofi-ttv
    chmod a+x $out/bin/rofi-ttv
  '';

  postInstall = ''
    substituteInPlace $out/bin/rofi-ttv \
      --replace "curl" "${pkgs.curl}/bin/curl" \
      --replace "jq" "${pkgs.jq}/bin/jq" \
      --replace "rofi " "${pkgs.rofi-wayland}/bin/rofi " \
      --replace "youtube-dl" "${pkgs.youtube-dl}/bin/youtube-dl" \
      --replace "mpv" "${pkgs.mpv}/bin/mpv" \
      --replace "ncat" "${pkgs.nmap}/bin/ncat" \
      --replace "nc " "${pkgs.netcat}/bin/nc "
  '';
}
