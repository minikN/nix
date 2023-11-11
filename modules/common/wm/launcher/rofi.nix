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
### Rofi configuration
###
### CODE:

{ config, lib, pkgs, ... }:

{
  imports = [
    ../launcher
  ];

  config = let

    ## Using the correct package based on the window system
    rofiPkg = if config.os.wayland
      then pkgs.rofi-wayland
      else pkgs.rofi;
  in {

    ## Telling the config about the launcher
    os.launcher.pkg = rofiPkg;
    os.launcher.name = "rofi";
    os.launcher.args = "-show drun";
    os.launcher.configFile = config.home-manager.users.${config.user}.programs.rofi.configPath;

    ## Configuration
    home-manager.users.${config.user} = {
    programs.rofi = {
        enable = true;
        package = rofiPkg;
        theme = "Arc";
        font = "${config.os.fonts.mono.light} ${builtins.toString config.os.fonts.size}";
      
        extraConfig = {
          show-actions = true;
          show-icons = true;

          ## Keybindingst
          kb-element-next = "";
          kb-row-select = "Tab,Control+i";
          kb-secondary-paste = "Control+y";
          kb-remove-word-forward = "Alt+d";
          kb-remove-word-backward = "Control+w,Control+Backspace";
          kb-clear-line = "Control+slash";
          kb-page-next = "Control+v";
          kb-page-prev = "Alt+v";
        };
      };
    };
  };
}

