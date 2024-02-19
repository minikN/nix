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
### Emacs orderless configuration
###
### CODE:

{ config, lib, pkgs, inputs, ... }:

{
  config = let
    utils = import ./../../../../utils.nix { inherit lib pkgs config; };
  in {
    home-manager.users.${config.user} = {
      programs.emacs = utils.emacsPkg {
        name = "db-orderless";
        description = "Orderless configuration";
        require = true;
        packages = [ pkgs.emacsPackages.orderless ];
        code = ''
;; ~!emacs-lisp!~
;; nothing yet.
'';
        config = ''
;; ~!emacs-lisp!~
(use-package orderless
  :custom
  (completion-styles '(basic orderless))
  (completion-category-overrides
   `((file (styles basic partial-completion orderless))
     (imenu (styles basic substring orderless))
     (kill-ring (styles emacs22 orderless))
     (eglot (styles emacs22 substring orderless))
     
     ;; conditionally add orderless if certain packages are installed.
     ,(if (package-installed-p 'embark)
	  (embark-keybinding (styles basic substring)))
     ,(if (package-installed-p 'consult)
	  (consult-location (styles basic substring orderless))))))
'';
      };
    };
  };
}
