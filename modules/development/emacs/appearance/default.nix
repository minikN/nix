

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
### Emacs appearance configuration
### Source: https://github.com/abcdw/rde/blob/master/src/rde/features/emacs-xyz.scm
###
### CODE:

{ config, lib, pkgs, ... }:

{
  imports = [
    ./theme.nix
    ./fonts.nix
  ];

  config = {
    home-manager.users.${config.user} = {
      imports = with config.nur.repos.rycee.hmModules; [ emacs-init ];
      programs.emacs.init = {
        enable = true;
        earlyInit = ''
;; ~!emacs-lisp!~
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(push '(horizontal-scroll-bars) default-frame-alist)
(push (cons 'left-fringe 8) default-frame-alist)
(push (cons 'right-fringe 8) default-frame-alist)
(push '(no-special-glyphs) default-frame-alist)
(push '(undecorated) default-frame-alist)
(setq menu-bar-mode nil
      tool-bar-mode nil
      scroll-bar-mode nil)
(push '(internal-border-width . 8) default-frame-alist)
(setq inhibit-startup-screen t
      inhibit-startup-message t
      initial-scratch-message nil)

;; suppress byte-compile warnings
(setq byte-compile-warnings nil)
        '';
      };

      programs.emacs = {
        extraPackages = epkgs: [ epkgs.minions ];
        extraConfig = ''
          (set-default 'cursor-type  '(bar . 1))
          (blink-cursor-mode 1)
          (setq-default cursor-in-non-selected-windows nil)
          (setq bookmark-set-fringe-mark nil)

          (with-eval-after-load 'minions-autoloads
            (minions-mode))
          (with-eval-after-load 'minions
            (setq minions-mode-line-lighter ";"))

          (setq mode-line-compact 'long)

          ;; Modus theme adapations
          (with-eval-after-load 'modus-themes
            (setq modus-themes-common-palette-overrides
                  '((border-mode-line-active unspecified)
                    (border-mode-line-inactive unspecified)
                    (fringe unspecified)
                    (fg-line-number-inactive "gray50")
                    (fg-line-number-active fg-main)
                    (bg-line-number-inactive unspecified)
                    (bg-line-number-active unspecified)
                    (bg-region bg-ochre)
                    (fg-region unspecified))))

          ;; Mode line at the top
          (setq minions-mode-line-minor-modes-map
                      (let ((map (make-sparse-keymap)))
                        ;; Make minions menu work in header line
                        (define-key map (vector 'header-line 'down-mouse-1)
                          'minions-minor-modes-menu)
                        map))
                (defun db--move-mode-line-to-header ()
                  "Move mode-line to header-line.
This function is needed for various modes to set up the mode-line late."
                  (setq-local header-line-format mode-line-format)
                  (setq-local mode-line-format nil))

                (add-hook 'calendar-initial-window-hook
                          'db--move-mode-line-to-header)
                (setq-default header-line-format mode-line-format)
                (setq-default mode-line-format nil)
                (setq mode-line-format nil)

                (with-eval-after-load 'menu-bar
                  (menu-bar-mode 0))
                (with-eval-after-load 'tool-bar
                  (tool-bar-mode 0))
                (with-eval-after-load 'scroll-bar
                  (scroll-bar-mode 0))
                (with-eval-after-load 'fringe
                  (fringe-mode 8))

                (set-frame-parameter (selected-frame) 'internal-border-width 8)
                (setq use-dialog-box nil)
                (setq use-file-dialog nil)
                (setq window-divider-default-right-width 8)
                (window-divider-mode)
        '';
      };
    };
  };
}

