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
  imports = [
    ./appearance
    ./keymaps.nix
    ./denote.nix
    ./code/eglot.nix
    ./code/dape.nix

    ./vertico.nix
    ./completion.nix

    ./code/lang/nix.nix

    ./edit/edit-indirect.nix

    ./windows.nix
    ./which-key.nix
  ];
  
  config = {
    home-manager.users.${config.user} = {
      programs.emacs = {
        enable = true;
        package = pkgs.emacs-pgtk;
        extraPackages = epkgs: [
          epkgs.magit
          epkgs.wgrep
          epkgs.treesit-grammars.with-all-grammars
        ];
        extraConfig = ''
;; ~~!emacs-lisp!~~
;; Packages will be initialized by guix later.
(setq package-enable-at-startup nil)
(setq package-archives nil)

;; Defer garbage collection further back in the startup process
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq undo-limit (* 8 1024 1024)
                  read-process-output-max (* 1024 1024))))

;; Ignore X resources
(advice-add #'x-apply-session-resources :override #'ignore)
;; TODO: Probably the better approach is:
;; (setq inhibit-x-resources t)

;; Do not resize the frame at this early stage.
(setq frame-inhibit-implied-resize t)

(pixel-scroll-precision-mode 1)

;; Set some defaults for the startup behaviour.
(setq use-dialog-box t
      use-file-dialog nil)

(defgroup db nil
  "Base customization group for user settings."
  :group 'external
  :prefix 'db-)

;; Theme
(load-theme 'modus-operandi t)

;; use-package
(setq use-package-always-defer t)


;; User information
(setq user-full-name "${config.fullName}"
      user-mail-address "${config.mail.primary.address}")

;; Directories
          ;;; Backup
(setq backup-directory-alist
      `(,(cons "." "${config.home-manager.users.${config.user}.xdg.cacheHome}/emacs/backup")))

          ;;; recentf
(recentf-mode 1)
(run-with-idle-timer 30 t 'recentf-save-list)
(setq recentf-save-file "${config.home-manager.users.${config.user}.xdg.cacheHome}/emacs/recentf")

;; savehist
(savehist-mode 1)
(run-with-idle-timer 30 t 'savehist-save)
(setq savehist-file "${config.home-manager.users.${config.user}.xdg.cacheHome}/emacs/history")

;; bookmarks
(setq bookmark-default-file "${config.home-manager.users.${config.user}.xdg.cacheHome}/emacs/bookmarks")

;; Font
(add-to-list 'default-frame-alist '(font . "${config.os.fonts.mono.regular}-${builtins.toString config.os.fonts.size}"))
(set-face-attribute 'default t :font "${config.os.fonts.mono.regular}-${builtins.toString config.os.fonts.size}")
        '';
      };
    };
  };
}


