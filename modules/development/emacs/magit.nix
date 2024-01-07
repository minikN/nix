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
### Emacs magit configuration
###
### CODE:


{ config, lib, pkgs, inputs, ... }:

{
  config = {
    home-manager.users.${config.user} = {
      programs.emacs = {
        extraPackages = epkgs: [ epkgs.magit ];
        extraConfig = ''
;; ~!emacs-lisp!~

(defgroup db-magit nil
  "Configuration related to `magit'."
  :group 'db)

(defcustom db-magit-worktree-dir "~/.local/share/git/worktrees/"
  "Default directory to create worktrees in."
  :type 'string
  :group 'db-magit)

(defun db--magit-worktree-dir (prompt)
  ""
  (let ((target-dir (concat
		     db-magit-worktree-dir
		     (file-name-nondirectory
		      (directory-file-name
		       (file-name-directory (pwd))))
		     "/")))
    (mkdir target-dir t)
    (read-directory-name prompt target-dir)))

(use-package magit
  :custom
  (magit-worktree-read-directory-name-function #'db--magit-worktree-dir))
        '';
      };
    };
  };
}


