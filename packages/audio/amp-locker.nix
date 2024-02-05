{ stdenv, pkgs, lib, ... }:

stdenv.mkDerivation rec {
  pname = "amp-locker";
  version = "1.0.7";

  src = pkgs.requireFile {
    message = "run nix-store --add-fixed sha256 ./AmpLockerLinux.zip";
    name = "AmpLockerLinux.zip";
    sha256 = "fcbed9c1a82260e8efa009b4fbe1c5104fa950b50cd2b5e26b4c67ecaf1d494e";
  };

  nativeBuildInputs = [ pkgs.makeWrapper pkgs.unzip ];
  buildInputs = [
    pkgs.freetype
    pkgs.libglvnd
    pkgs.glibc
    pkgs.alsa-lib
    stdenv.cc.cc.lib
    pkgs.curlWithGnuTls
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
$out/lib/vst3 \
$out/opt \
"$out/opt/Audio Assault"

cp -r AmpLockerData "$out/opt/Audio Assault/"
cp "Amp Locker vst2.so" $out/lib/vst/
cp -r "Amp Locker.vst3" $out/lib/vst3/
cp "Amp Locker Standalone" $out/bin/amp-locker
  '';

  postInstall = ''
exe=$out/bin/amp-locker
data=$out/opt/Audio\ Assault/AmpLockerData

chmod -x $exe
chmod -R 775 $data
'';

  postFixup = ''
    patchelf --set-interpreter "$(cat $NIX_CC/nix-support/dynamic-linker)" "$out/bin/amp-locker"

    for file in \
      "$out/bin/amp-locker" \
      "$out/lib/vst/Amp Locker vst2.so" \
      "$out/lib/vst3/Amp Locker.vst3/Contents/x86_64-linux/Amp Locker.so"
    do
      patchelf --set-rpath "${lib.makeLibraryPath buildInputs}" "$file"
    done

wrapProgram $out/bin/amp-locker \
  --set LD_PRELOAD "${pkgs.libredirect}/lib/libredirect.so" \
  --set NIX_REDIRECTS "/opt=$out/opt"
  '';
}
