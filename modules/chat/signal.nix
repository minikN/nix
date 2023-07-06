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
### Signal Desktop configuration
###
### CODE:

{ config, lib, pkgs, ... }:

{
  config = {
    ## Overlaying the package if we're on wayland
    nixpkgs.overlays = lib.mkIf config.os.wayland [
       (self: super: {
        signal-desktop = super.signal-desktop.overrideAttrs (old: {
          preFixup = old.preFixup + ''
            gappsWrapperArgs+=(
              --add-flags "--enable-features=UseOzonePlatform"
              --add-flags "--ozone-platform=wayland"
            )
          '';
        });
      })
    ];
    
    ## Installing the package
    home-manager.users.${config.user}.home.packages = [ pkgs.signal-desktop ];
  };
}

