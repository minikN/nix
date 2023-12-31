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
### Emacs theme configuration
### Source: https://github.com/abcdw/rde/blob/master/src/rde/features/emacs-xyz.scm
###
### CODE:

{ config, lib, pkgs, ... }:

{
  config = {
    home-manager.users.${config.user} = {
      programs.emacs = {
        extraPackages = epkgs: [ epkgs.modus-themes ];
        extraConfig = ''
;; ~!emacs-lisp!~
(eval-when-compile
  (require 'modus-themes)
  (require 'cl-seq))
(require 'modus-operandi-theme)
(eval-when-compile
  (enable-theme 'modus-operandi))
(defgroup db-modus-themes nil
  "Configuration related to `modus-themes'."
  :group 'db)
(defcustom db-modus-themes-mode-line-padding 1
  "The padding of the mode line."
  :type 'number
  :group 'db-modus-themes)
(defcustom db-modus-themes-tab-bar-padding 1
  "The padding of the tab bar."
  :type 'number
  :group 'db-modus-themes)
(defcustom db-modus-themes-header-line-padding 1
  "The padding of the header line."
  :type 'number
  :group 'db-modus-themes)
(defcustom db-modus-themes-after-enable-theme-hook nil
  "Normal hook run after enabling a theme."
  :type 'hook
  :group 'db-modus-themes)

(defun db-modus-themes-run-after-enable-theme-hook (&rest _args)
  "Run `db-modus-themes-after-enable-theme-hook'."
  (run-hooks 'db-modus-themes-after-enable-theme-hook))

(defun db-modus-themes-set-custom-faces (&optional _theme)
  "set faces based on the current theme."
  (interactive)
  (when (modus-themes--current-theme)
    (modus-themes-with-colors
      (custom-set-faces
       `(window-divider ((,c :foreground ,bg-main)))
       `(window-divider-first-pixel ((,c :foreground ,bg-main)))
       `(window-divider-last-pixel ((,c :foreground ,bg-main)))
       `(vertical-border ((,c :foreground ,bg-main)))
       `(tab-bar
         ((,c :background ,bg-dim
              :box (:line-width ,db-modus-themes-tab-bar-padding
				:color ,bg-dim))))
       `(mode-line
         ((,c :box (:line-width ,db-modus-themes-mode-line-padding
				:color ,bg-mode-line-active))))
       `(mode-line-inactive
         ((,c :box (:line-width ,db-modus-themes-mode-line-padding
				:color ,bg-mode-line-inactive))))
       `(header-line
         ((,c :box (:line-width ,db-modus-themes-header-line-padding
				:color ,bg-dim))))
       `(git-gutter-fr:added
         ((,c :foreground ,bg-added-fringe :background ,bg-main)))
       `(git-gutter-fr:deleted
         ((,c :foreground ,bg-removed-fringe :background ,bg-main)))
       `(git-gutter-fr:modified
         ((,c :foreground ,bg-changed-fringe :background ,bg-main)))
       `(aw-leading-char-face
         ((,c :height 1.0 :foreground ,blue-cooler)))))))

(defun db-modus-themes--dark-theme-p (&optional theme)
  "Indicate if there is a curently-active dark THEME."
  (if theme
      (eq theme 'modus-operandi)
    (eq (car custom-enabled-themes) 'modus-vivendi)))

(setq db-modus-themes-header-line-padding 4)
(setq db-modus-themes-tab-bar-padding 4)
(setq db-modus-themes-mode-line-padding 4)

(advice-add 'enable-theme
            :after 'db-modus-themes-run-after-enable-theme-hook)

(add-hook 'db-modus-themes-after-enable-theme-hook 'db-modus-themes-set-custom-faces)

(with-eval-after-load
    'db-keymaps
  (define-key db-toggle-map (kbd "t") 'modus-themes-toggle))

(eval-when-compile
  (setq modus-themes-common-palette-overrides
        '((border-mode-line-active unspecified)
          (border-mode-line-inactive unspecified)
          (fringe unspecified)
          (fg-line-number-inactive "gray50")
          (fg-line-number-active fg-main)
          (bg-line-number-inactive unspecified)
          (bg-line-number-active unspecified)
          (bg-region bg-ochre)
          (fg-region unspecified)))
  (setq modus-themes-to-toggle
        '(modus-operandi modus-vivendi))
  (setq modus-themes-italic-constructs t)
  (setq modus-themes-bold-constructs t)
  (setq modus-themes-mixed-fonts t)
  (setq modus-themes-org-blocks 'gray-background))
(load-theme 'modus-operandi t)
        '';
      };
    };
  };
}

