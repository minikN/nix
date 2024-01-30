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
      programs.emacs = {
        extraPackages = epkgs: [ epkgs.dape ];
        extraConfig = ''
;; ~!emacs-lisp!~
(use-package dape
  :config
  ;; Enables ability to click on fringe to create breakpoints
  ;;(dape-info-breakpoints-mode 1)
  
  ;; Add inline variable hints, this feature is highly experimental
  ;; (setq dape-inline-variables t)

  ;; To remove info buffer on startup
  ;; (remove-hook 'dape-on-start-hooks 'dape-info)

  ;; To remove repl buffer on startup
  ;; (remove-hook 'dape-on-start-hooks 'dape-repl)

  ;; By default dape uses gdb keybinding prefix
  ;; (setq dape-key-prefix "\C-x\C-a")

  ;; Kill compile buffer on build success
  ;; (add-hook 'dape-compile-compile-hooks 'kill-buffer)

  ;; Customize actions in info buffer
  (setq dape-info-buttons
        '(("→" . dape-next)
          ("↘" . dape-step-in)
          ("↗" . dape-step-out)
          ("⯈" . dape-continue)
          ("⏸" . dape-pause)
          ("⭯" . dape-restart)
          ("x" . dape-quit))))

;; TODO: Fix dape font size
;; (dolist
;;     (hook
;;      '(dape-info-mode-hook
;;        dape-repl-mode-hook))
;;   (add-hook hook
;;             (lambda ()
;;               (face-remap-add-relative 'default :height 0.75))))
        '';
      };
    };
  };
}

