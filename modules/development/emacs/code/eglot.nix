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
### Emacs eglot configuration
### Source: https://github.com/abcdw/rde/blob/master/src/rde/features/emacs-xyz.scm#L154
###
### CODE:

{ config, lib, pkgs, inputs, ... }:

{ 
  config = {
    home-manager.users.${config.user} = {
      programs.emacs = {
        extraPackages = epkgs: [ epkgs.consult-eglot ];
        extraConfig = ''
;; ~!emacs-lisp!~
(with-eval-after-load
    'eglot
  (defun db--eglot-code-action-on-word ()
    (interactive)
    (left-word)
    (mark-word)
    (eglot-code-actions (point) (mark) nil t)
    (deactivate-mark))
  
  (setq eldoc-echo-area-use-multiline-p nil)
  (setq eglot-confirm-server-initiated-edits nil)
  ;; (add-hook 'eglot-managed-mode-hook
  ;; 	    (lambda ()
  ;; 	      (setq consult-imenu--cache nil)
  ;;             (defvar eglot-mode-command-map
  ;; 		(let ((map (make-sparse-keymap)))
  ;; 		  (define-key map (kbd "c") '("Actions" . eglot-code-actions))
  ;; 		  (define-key map (kbd "d") '("Find definition" . xref-find-definitions))
  ;; 		  (define-key map (kbd "D") '("Find declaration" . eglot-find-declaration))
  ;; 		  (define-key map (kbd "I") '("Find implementation" . eglot-find-implementation))
  ;; 		  (define-key map (kbd "R") '("Find references" . xref-find-references))
  ;; 		  (define-key map (kbd "S") '("Apropos symbol" . xref-find-apropos))
  ;; 		  (when (featurep 'consult)
  ;; 		    (define-key map (kbd "s") '("Search symbol" . consult-eglot-symbols)))
  ;; 		  (define-key map (kbd "t") '("Find type definition" . eglot-find-typeDefinition))
  ;; 		  (define-key map (kbd "f") '("Format region" . eglot-format))
  ;; 		  (define-key map (kbd "F") '("Format buffer" . eglot-format-buffer))
  ;; 		  (define-key map (kbd "o") '("Organize imports" . eglot-code-action-organize-imports))
  ;; 		  (define-key map (kbd "r") '("Rename" . eglot-rename))
  ;; 		  (define-key map (kbd "h") '("Show documentation" . eldoc-doc-buffer))
  ;; 	          (define-key map (kbd "e") '("Diagnostics" . flymake-show-buffer-diagnostics))
  ;; 		  (define-key map (kbd "e") '("Project diagnostics" . flymake-show-project-diagnostics))
  ;; 		  map))
  ;; 	      (fset 'eglot-mode-command-map eglot-mode-command-map)

  ;; 	      (define-key eglot-mode-map (kbd "C-c c") '("Code" . eglot-mode-command-map))
  ;; 	      (define-key eglot-mode-map (kbd "M-RET") 'db--eglot-code-action-on-word)
  
  ;; 	      ;; TODO: Move this to own module
  ;; 	      ;; Add flymake diagnostics to mode bar
  ;; 	      (add-to-list 'mode-line-misc-info
  ;; 			   `(flymake-mode (" " flymake-mode-line-counters " ")))))
  
  ;; Potentially can speed up eglot:
  (setq eglot-events-buffer-size 0)
  (setq eglot-extend-to-xref t))
        '';
      };
    };
  };
}

