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
### Firefox configuration
###
### CODE:

{ config, lib, pkgs, ... }:

{
  config = let
    ## Choosing the correct package depending on the window system in use
    firefoxPkg = if config.os.wayland
      then pkgs.firefox-wayland
      else pkgs.firefox;
  in {
    home-manager.users.${config.user} = {

      ## Setting the proper session variables for wayland
      home.sessionVariables = lib.mkIf config.os.wayland {
        MOZ_ENABLE_WAYLAND = "1";
      };

      ## Enabling firefox
      programs.firefox = {
        enable = true;
        package = firefoxPkg;

        ## Enabling some extensions by default
        profiles.${config.user}.extensions = with config.nur.repos.rycee.firefox-addons; [
          ublock-origin
        ];
      };
    };
  };
}

