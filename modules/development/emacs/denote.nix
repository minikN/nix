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
    (define-key db-app-map (kbd "N") 'denote)))
        '';
      };
    };
  };
}


