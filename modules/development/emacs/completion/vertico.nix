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
### Emacs vertico configuration
###
### CODE:

{ config, lib, pkgs, inputs, ... }:

{
  config = let
    utils = import ./../../../../utils.nix { inherit lib pkgs config; };
  in {
    home-manager.users.${config.user} = {
      programs.emacs = utils.emacsPkg {
        name = "db-vertico";
        description = "Vertico configuration";
        require = true;
        packages = [ pkgs.emacsPackages.vertico ];
        code = ''
;; ~!emacs-lisp!~
(defvar db-vertico-multiform-maximal
  '((vertico-count . 10)
    (vertico-resize . t))
  "List of configurations for maximal Vertico multiform.")

;; Source: https://protesilaos.com/emacs/dotemacs#h:3796a4c9-8659-4782-8aaa-3cf4e950927d
(defun db-vertico--match-directory (str)
  "Match directory delimiter in STR."
  (string-suffix-p "/" str))

;; From the Vertico documentation.
(defun db-vertico-sort-directories-first (files)
  "Sort directories before FILES."
  (setq files (vertico-sort-alpha files))
  (nconc (seq-filter #'db-vertico--match-directory files)
         (seq-remove #'db-vertico--match-directory files)))
'';
        config = ''
;; ~!emacs-lisp!~
(use-package vertico
  :init
  (setq vertico-scroll-margin 0)
  (setq vertico-count 5)
  (setq vertico-resize nil)
  (setq vertico-cycle t)

  (setq vertico-multiform-categories
	;; Completion categories to display in vertico
        `((multi-category ,@db-vertico-multiform-maximal)
	  (file ,@db-vertico-multiform-maximal
		(vertico-preselect . prompt)
                (vertico-sort-function . db-vertico-sort-directories-first))))
  :config
  (vertico-mode 1)
  (vertico-multiform-mode 1))
'';
      };
    };
  };
}
