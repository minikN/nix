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
### Emacs edit-indirect configuration
###
### CODE:

{ config, lib, pkgs, inputs, ... }:

{
  config = {
    home-manager.users.${config.user} = {
      programs.emacs = {
        extraPackages = epkgs: [ epkgs.edit-indirect ];
        extraConfig = ''
;; ~!emacs-lisp!~
(defun db-edit-in-indirect-buffer (&optional delimiter)
  (interactive)
  (let ((delim (or delimiter (read-from-minibuffer "Delimiter: "))))
    (save-excursion
      (db-mark-text-between-delimiter delim)
      (edit-indirect-region (point) (mark) t))))

(use-package edit-indirect
  :config
  (add-hook
   'edit-indirect-after-commit-functions
   (lambda (beg end)
     (interactive)
     (deactivate-mark)))
  (advice-add 'edit-indirect-abort :after #'deactivate-mark)
  :hook ((edit-indirect-before-commit . db-indent-buffer)))

(use-package nix-mode
  :config
  ;; TODO: Make it work with :bind
  (with-eval-after-load 'nix-mode
    (define-key nix-mode-map
 		(kbd "C-c C-'")
 		'("Edit in major mode" . (lambda ()
 					   (interactive)
 					   (db-edit-in-indirect-buffer "\'\'"))))))

(defgroup db-edit nil
  "Tweaks to emacs' editing functionalities."
  :group 'db)

(defcustom db-edit-delimiter-mode-alist
  '(("~!emacs-lisp!~" . emacs-lisp-mode)
    ("~!shell!~" . shell-script-mode))
  "An association list that binds a specific delimiter
    to a major mode. Used for guessing the correct mode
    while using `edit-indirect'."
  :type 'list
  :group 'db-edit)

(defun db-edit-guess-mode (buffer beg end)
  "Function to guess the mode of the current
buffer based on a predefined delimiter."
  (mapcar (lambda (el)
	    (let ((delimiter (car el))
		  (mode (cdr el)))
	      (with-current-buffer (current-buffer)
		(goto-char (point-min))
		(when (search-forward delimiter nil t)
		  (funcall mode)))))
	  db-edit-delimiter-mode-alist))

;; Specifying which function should be called when opening
;; an indirect buffer with `edit-indirect' to guess the
;; correct mode. The function will use the custom variable
;; `db-edit-indirect-delimiter-mode-alist'.
(setq edit-indirect-guess-mode-function #'db-edit-guess-mode)

(defun db-mark-text-between-delimiter (delimiter)
  "Marks text between DELIMITER, which should be
a literal string."
  (interactive)
  (let ((delimiter-length (length delimiter))
	(start)
	(end))
    (search-backward delimiter)
    (next-line 1)
    (beginning-of-line)
    (setq start (point))
    (search-forward delimiter)
    (previous-line 1)
    (end-of-line)
    (setq end (point))
    (set-mark start)))

(defun db-indent-buffer ()
  "Indents the whole buffer."
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max) nil)))
'';
      };
    };
  };
}
