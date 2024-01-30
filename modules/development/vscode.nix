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
    package = pkgs.vscode;
    
    userSettings = {
      "window.titleBarStyle" = "custom"; 

      ## No auto-sync
      "git.confirmSync" = false;
      
      ## Auto save
      "files.autoSave" = "onFocusChange";

      ## No tabs
      "workbench.editor.showTabs" = "none";

      ## Indentation
      "editor.tabSize" =  2;
      "editor.detectIndentation" = false;

      ## Font
      "editor.fontFamily" = "'${config.os.fonts.mono.regular}', 'monospace', monospace";
      "editor.fontSize" = config.os.fonts.size + 2;
      "editor.fontLigatures" = true;

      # typscript
      #"typescript.tsserver.log" = "verbose";
      "typescript.tsdk" = "${pkgs.nodePackages.typescript}/lib/node_modules/typescript/lib";

      # Vim
      "vim.handleKeys" = {
        "<C-e>" = false;
        "<C-p>" = false;
      };
    };

    keybindings = [
      { key = "ctrl+shift+alt+p"; command = "eslint.executeAutofix"; }
      {
        key = "ctrl+alt+o";
        command = "editor.action.organizeImports";
        when = "textInputFocus && !editorReadonly && supportedCodeAction =~ /(\\s|^)source\\.organizeImports\\b/";
      }
    ];
  };
}

