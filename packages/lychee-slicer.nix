{ pkgs, ... }:

let
  version = "5.2.2";
in
pkgs.appimageTools.wrapType2 {
  pname = "lychee-slicer";
  inherit version;

  src = pkgs.fetchurl {
    url = "https://mango-lychee.nyc3.cdn.digitaloceanspaces.com/LycheeSlicer-${version}.AppImage";
    sha256 = "sha256-INRo3SYHPffFPTFAYGlqh09jpeA+ropWAnwqzvkbF7w=";
  };

  profile = ''
    export LC_ALL=C.UTF-8
    export XDG_DATA_DIRS="${pkgs.gsettings-desktop-schemas}/share/gsettings-schemas/${pkgs.gsettings-desktop-schemas.name}:${pkgs.gtk3}/share/gsettings-schemas/${pkgs.gtk3.name}:$XDG_DATA_DIRS"
  '';

  extraPkgs = pkgs: (pkgs.appimageTools.defaultFhsEnvArgs.multiPkgs pkgs) ++ (with pkgs; [
    # fixes "unexpected error"
    gsettings-desktop-schemas glib gtk3

    # needed for icons
    gnome.adwaita-icon-theme
  ]);
}