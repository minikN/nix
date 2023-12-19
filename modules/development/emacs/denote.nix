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
### Emacs denote configuration
###
### CODE:


{ config, lib, pkgs, inputs, ... }:

{
  config = {
    home-manager.users.${config.user} = {
      programs.emacs = {
        extraPackages = epkgs: [ epkgs.denote ];
        extraConfig = ''
;; ~!emacs-lisp!~

(use-package denote
  :init
  (defvar denote-command-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "n") '("Note" . denote))
      (define-key map (kbd "d") '("Note (Date)" . denote-date))
      (define-key map (kbd "t") '("Note (Template)" . denote-template))
      (define-key map (kbd "T") '("Note (Type)" . denote-type))
      (define-key map (kbd "s") '("Note (Signature)" . denote-signatureo))
      (define-key map (kbd "l") '("Add link" . denote-link))
      (define-key map (kbd "b") '("Show backlinks" . denote-backlinks))
      (define-key map (kbd "L") '("Add links" . denote-add-links))
      map)
    "Keymap for denote.")

  (fset 'denote-command-map denote-command-map)
  
  :config
  ;; Remember to check the doc strings of those variables.
  (setq denote-directory (expand-file-name "~/Documents/notes/"))
  (setq denote-known-keywords '("emacs" "todo" "nix"))
  (setq denote-infer-keywords t)
  (setq denote-sort-keywords t)
  (setq denote-file-type nil) ; Org is the default, set others here
  (setq denote-prompts '(title keywords))
  (setq denote-excluded-directories-regexp nil)
  (setq denote-excluded-keywords-regexp nil)
  (with-eval-after-load
      'db-keymaps
    (define-key db-app-map (kbd "N") '("Notes" . denote-command-map))))
        '';
      };
    };
  };
}


