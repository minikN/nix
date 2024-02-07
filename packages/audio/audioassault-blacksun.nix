{ stdenv, pkgs, lib, ... }:

stdenv.mkDerivation rec {
  pname = "audioassault-blacksun";
  version = "1.0.0";

  src = pkgs.requireFile {
    message = "run nix-store --add-fixed sha256 ./Blacksun+Installers+v1.zip";
    name = "Blacksun+Installers+v1.zip";
    sha256 = "42f612cec63857d9cc85a2a2886c17eb02c7640032c5ef07098f2b895a8b1c6f";
  };

  nativeBuildInputs = [ pkgs.makeWrapper pkgs.unzip ];
  buildInputs = [
    pkgs.freetype
    pkgs.libglvnd
    pkgs.glibc
    pkgs.alsa-lib
    stdenv.cc.cc.lib
    pkgs.curlWithGnuTls
    pkgs.xorg.libX11
    pkgs.xorg.libXext
  ];

  dontStrip = true;

  unpackPhase = ''
    unzip $src
  '';

  installPhase = ''
mkdir -p $out \
$out/bin \
$out/lib \
$out/lib/vst \
$out/opt \
"$out/opt/Audio Assault"

cp -r "Blacksun Linux/Blacksun" "$out/opt/Audio Assault/"
cp "Blacksun Linux/Blacksun Standalone" "$out/bin/audioassault-blacksun"
cp "Blacksun Linux/Blacksunvst2.so" "$out/lib/vst/"
cp -r BStarA412\ FREE\ IR\ PACK/* "$out/opt/Audio Assault/Blacksun/IRs/"

find $out -name ".DS_Store" -delete
find $out -name "*.pdf" -delete
  '';

  postInstall = ''
exe=$out/bin/audioassault-blacksun
data=$out/opt/Audio\ Assault/Blacksun

chmod -x $exe
chmod -R 775 $data
'';

  postFixup = ''
patchelf --set-interpreter "$(cat $NIX_CC/nix-support/dynamic-linker)" "$out/bin/audioassault-blacksun"

for file in \
  "$out/bin/audioassault-blacksun" \
  "$out/lib/vst/Blacksunvst2.so"
do
  patchelf --set-rpath "${lib.makeLibraryPath buildInputs}" "$file"
done

wrapProgram $out/bin/audioassault-blacksun \
  --set LD_PRELOAD "${pkgs.libredirect}/lib/libredirect.so" \
  --set NIX_REDIRECTS "/opt=$out/opt"
  #--set LD_LIBRARY_PATH "${lib.makeLibraryPath buildInputs}"
'';
}
