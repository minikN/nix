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
### Emacs consult configuration
###
### CODE:

{ config, lib, pkgs, inputs, ... }:

{
  config = let
    utils = import ./../../../../utils.nix { inherit lib pkgs config; };
  in {
    home-manager.users.${config.user} = {
      programs.emacs = utils.emacsPkg {
        name = "db-consult";
        description = "Consult configuration";
        require = true;
        packages = [ pkgs.emacsPackages.consult ];
        code = ''
;; ~!emacs-lisp!~
;; Nothing yet.
'';
        config = ''
;; ~!emacs-lisp!~
;; TODO: remap keybindings; look into yank/pop
(use-package consult
  :config
  ;; Addings consult completion categories to vertico
  (with-eval-after-load 'db-vertico
    (setq vertico-multiform-categories
	  (append `((consult-location ,@db-vertico-multiform-maximal)
		    (consult-grep ,@db-vertico-multiform-maximal))
		  vertico-multiform-categories))))
'';
      };
    };
  };
}
