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
### tessen configuration
###
### CODE:

{ config, lib, pkgs, ... }:

{
  config = {

    ## Informing nix about the current password manager
    os.passwordManager = "${pkgs.tessen}/bin/tessen";

    home-manager.users.${config.user} = {
      home.packages = [ pkgs.tessen ];

      ## Writing tessen's config file
      ## The use of 'launcher` makes tessen work with every launcher
      ## as long as it's supported by it
      xdg.configFile."tessen/config".text = ''
        dmenu_backend="${config.os.launcher.name}"
        ${config.os.launcher.name}_config_file="${config.os.launcher.configFile}"
      '';
    };
  };
}

