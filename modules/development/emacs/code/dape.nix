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
### Emacs dape configuration
### Source: https://github.com/abcdw/rde/blob/master/src/rde/features/emacs-xyz.scm#L154
###
### CODE:

{ config, lib, pkgs, inputs, ... }:

{ 
  config = {
    home-manager.users.${config.user} = {
      programs.emacs = let
        jsonrpc = pkgs.emacsPackages.trivialBuild {
          pname = "jsonrpc";
	        version = "0.1";
          src = pkgs.fetchFromGitHub {
            owner = "svaante";
            repo = "jsonrpc";
            rev = "7155e2ad12adb9820300cec339141f09e3e67a7b";
            sha256 = "sha256-3o8qPOPraiyr4iJWlv2h92NIC/dQKTMRfoPpGwFhyiE=";
          }; 
        };
      in {
        extraPackages = epkgs: [ epkgs.dape jsonrpc ];
        extraConfig = ''
;; ~!emacs-lisp!~
(use-package dape
  :config
  ;; Enables ability to click on fringe to create breakpoints
  (dape-breakpoint-global-mode)
  
  (setq dape-buffer-window-arrangement 'right)

  ;; To not display info and/or buffers on startup
  (remove-hook 'dape-on-start-hooks 'dape-info)
  (remove-hook 'dape-on-start-hooks 'dape-repl)

  ;; To display info and/or repl buffers on stopped
  (add-hook 'dape-on-stopped-hooks 'dape-info)
  (add-hook 'dape-on-stopped-hooks 'dape-repl)

  ;; TODO: Fix dape font size
  ;; (dolist
  ;;     (hook
  ;;      '(dape-info-mode-hook
  ;;        dape-repl-mode-hook))
  ;;   (add-hook hook
  ;;             (lambda ()
  ;;               (face-remap-add-relative 'default :height 0.75))))
  )
        '';
      };
    };
  };
}

