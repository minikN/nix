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
### Enables modules commonly used for music
###
### CODE:

{ config, lib, pkgs, ... }:

{
  imports = [
    ## Reverse import the wrapper feature so that the options are defined.
    ../features
  ];

  config = {

    features.music = true;

    nixpkgs.overlays = [
      (self: super: {
        amp-locker = super.callPackage ../packages/audio/amp-locker.nix { };
        amp-locker-desktop = super.writeTextDir "share/applications/amp-locker.desktop" ''
          [Desktop Entry]
          Version=0.1.0
          Type=Application
          Name=Amp Locker
          Exec=${pkgs.amp-locker}/bin/amp-locker
        '';
      })
    ];

    home-manager.users.${config.user}.home = {
      packages = with pkgs; [
        amp-locker
        amp-locker-desktop
      ];

      file.".vst/Amp Locker vst2.so".source = "${pkgs.amp-locker}/lib/vst/Amp Locker vst2.so";
      file.".vst3/Amp Locker.vst3".source = "${pkgs.amp-locker}/lib/vst3/Amp Locker.vst3";
    };
  };
}
