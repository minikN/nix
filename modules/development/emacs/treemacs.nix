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
### Emacs treemacs configuration
###
### CODE:

{ config, lib, pkgs, inputs, ... }:

{
  config = let
    utils = import ./../../../utils.nix { inherit lib pkgs config; };
  in {
    home-manager.users.${config.user} = {
      programs.emacs = utils.emacsPkg {
        name = "db-treemacs";
        description = "Treemacs configuration";
        require = true;
        packages = [ pkgs.emacsPackages.treemacs ];
        code = ''
;; ~!emacs-lisp!~
(defun db-treemacs-open-project ()
  "Opens the Treemacs buffer for the current project if
and only if a project buffer is currently displayed. If not,
it will close the Treemacs window if one is displayed
already"
  (interactive)
  (if (project-current)
      (if (db--get-matching-window "\\*Treemacs.*")
	  (delete-window (treemacs-get-local-window))
	(progn
	  (treemacs-add-and-display-current-project-exclusively)
	  (other-window 1)))
    (if (db--get-matching-window "\\*Treemacs.*")
	(delete-window (treemacs-get-local-window)))))

;;; TODO: Add Git branch / list of staged/unstaged files to header-line
(defun db-treemacs-set-header-line ()
  "Sets the header-line-format of the Treemacs buffer to
the currently displayed project's name."
  (interactive)
  (let ((header-line (setq treemacs-user-header-line-format
			   (format "%s"
				   (treemacs-workspace->name
				    (car (treemacs-workspace->projects
					  (treemacs-current-workspace))))))))
    (setf
     (buffer-local-value
      'header-line-format
      (db--get-matching-buffer "\\*Treemacs.*"))
     header-line)))
'';
        config = ''
;; ~!emacs-lisp!~
(use-package treemacs
  :bind (("M-1" . db-treemacs-open-project))
  :after (db-windows)
  :config
  (advice-add
   'treemacs--follow-after-buffer-list-update
   :after #'db-treemacs-set-header-line)
  (setq treemacs-user-mode-line-format 'none)
  (treemacs-project-follow-mode 1)
  (treemacs-git-mode 'simple))
'';
      };
    };
  };
}
