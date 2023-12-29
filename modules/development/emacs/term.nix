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
### Emacs terminal configuration
###
### CODE:


{ config, lib, pkgs, inputs, ... }:

{
  config = let
    utils = import ./../../../utils.nix { inherit lib pkgs config; };
  in {
    home-manager.users.${config.user}.programs.emacs = utils.emacsPkg {
      name = "db-terminal";
      description = "Terminal configuration";
      require = true;
      packages = [ pkgs.emacsPackages.vterm ];
      config = ''
;; ~!emacs-lisp!~
(use-package vterm
  :init
  (add-to-list 'project-switch-commands '(project-vterm "Terminal") t)
  (add-to-list 'project-kill-buffer-conditions '(major-mode . vterm-mode))
  (define-key project-prefix-map (kbd "t") '("Terminal" . project-vterm)))

(with-eval-after-load 'db-keymaps
  (define-key db-app-map (kbd "t") '("Terminal" . vterm)))
'';
      code = ''
;; ~!emacs-lisp!~

(defun project-vterm ()
  "Open a vterm instance in the root of the current project"
  (interactive)
  (defvar vterm-buffer-name)
  (let* ((default-directory (project-root (project-current t)))
         (vterm-buffer-name (project-prefixed-buffer-name "vterm"))
         (vterm-buffer (get-buffer vterm-buffer-name)))
    (if (and vterm-buffer (not current-prefix-arg))
        (pop-to-buffer vterm-buffer (bound-and-true-p display-comint-buffer-action))
      (vterm))))
'';
    };
  };
}


