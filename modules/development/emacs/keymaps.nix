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
### Emacs configuration
###
### CODE:


{ config, lib, pkgs, inputs, ... }:

{
  config = {
    home-manager.users.${config.user} = {
      programs.emacs = let
        keymaps = pkgs.emacsPackages.trivialBuild {
          pname = "${config.user}-keymaps";
          version = "0.1";
          src = pkgs.writeText "${keymaps.pname}.el" ''
            ;;; ${keymaps.pname}.el --- Window configuration -*- lexical-binding: t -*-
              (defvar ${config.user}-app-map nil "Prefix keymap for applications.")
              (define-prefix-command '${config.user}-app-map nil)
              (defvar ${config.user}-toggle-map nil "\
          Prefix keymap for binding various minor modes for toggling functionalitty.")
              (define-prefix-command '${config.user}-toggle-map nil)  
            (provide '${keymaps.pname})
          '';
        };
      in {
        extraPackages = epkgs: [ keymaps ];
        extraConfig = ''
          (require '${keymaps.pname})

          (define-key mode-specific-map (kbd "a")
            '("applications" . ${config.user}-app-map))
          (define-key mode-specific-map (kbd "t")
            '("toggles" . ${config.user}-toggle-map))
        '';
      };
    };
  };
}


