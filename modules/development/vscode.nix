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
### VSCode configuration
###
### CODE:

{ config, lib, pkgs, ... }:

{
  home-manager.users.${config.user}.programs.vscode = {
    enable = true;
    userSettings = {
      ## No auto-sync
      "git.confirmSync" = false;
      
      ## Auto save
      "files.autoSave" = "onFocusChange";

      ## Indentation
      "editor.tabSize" =  2;
      "editor.detectIndentation" = false;

      ## Font
      "editor.fontFamily" = "'${config.os.fonts.mono.regular}', 'monospace', monospace";
      "editor.fontSize" = config.os.fonts.mono.size + 2;
      "editor.fontLigatures" = true;
    };
  };
}
