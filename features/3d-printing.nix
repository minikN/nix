### NixOS Configuration
###
### Copyright © 2023 Demis Balbach <db@minikn.xyz>
###
### This file is not part of Nix/NixOS/Home Manager.
###
### My config is free software; you can redistribute it and/or modify it
### under the terms of the GNU General Public License as published by
### the Free Software Foundation; either version 3 of the License, or (at
### your option) any later version.
###
### My config is distributed in the hope that it will be useful, but
### WITHOUT ANY WARRANTY; without even the implied warranty of
### MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
### GNU General Public License for more details.
###
### You should have received a copy of the GNU General Public License
### along with my config. If not, see <http://www.gnu.org/licenses/>.
###
### COMMENT:
###
### Enables modules commonly used for 3D printing
###
### CODE:

{ config, lib, pkgs, ... }:

{
  imports = [
    ## Reverse import the wrapper feature so that the options are defined.
    ../features
  ];

  config = {

    features."3d-printing" = true;

    nixpkgs.overlays = [
      (self: super: {
        lychee-slicer = super.callPackage ../packages/lychee-slicer.nix { };
        lychee-slicer-desktop = super.writeTextDir "share/applications/lychee-slicer.desktop" ''
          [Desktop Entry]
          Version=5.2.2
          Type=Application
          Name=Lychee Slicer
          Exec=${pkgs.lychee-slicer}/bin/lychee-slicer-5.2.2
        '';
      })
    ];

    home-manager.users.${config.user}.home.packages = with pkgs; [
      lychee-slicer
      lychee-slicer-desktop
      ## Installed 5.x via flatpak:
      ## flatpak remote-add --if-not-exists flathub https://flathub.org/repo/flathub.flatpakrepo
      ## flatpak update
      ## flatpak install flathub com.ultimaker.cura
      # cura
      blender
      prusa-slicer
    ];
  };
}