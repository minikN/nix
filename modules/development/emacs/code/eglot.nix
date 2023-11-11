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
### Emacs eglot configuration
### Source: https://github.com/abcdw/rde/blob/master/src/rde/features/emacs-xyz.scm#L154
###
### CODE:

{ config, lib, pkgs, inputs, ... }:

{ 
  config = {
    home-manager.users.${config.user} = {
      programs.emacs = {
        extraPackages = epkgs: [ epkgs.consult-eglot ];
        extraConfig = ''
          (with-eval-after-load
            'eglot
            (setq eldoc-echo-area-use-multiline-p nil)
            (setq eglot-confirm-server-initiated-edits nil)
            (add-hook 'eglot-managed-mode-hook
                      (lambda () (setq consult-imenu--cache nil)))
            ;; Potentially can speed up eglot:
            ;; (setq eglot-events-buffer-size 0)
            (setq eglot-extend-to-xref t))
        '';
      };
    };
  };
}

