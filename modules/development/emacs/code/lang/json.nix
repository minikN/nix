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
### Emacs json configuration
###
### CODE:

{ config, lib, pkgs, inputs, ... }:

{
  config = let
    utils = import ./../../../../../utils.nix { inherit lib pkgs config; };
  in {
    home-manager.users.${config.user} = {
      programs.emacs = let
        json-simple-flymake = pkgs.emacsPackages.trivialBuild {
          pname = "json-simple-flymake";
	        version = "1.0";
          src = pkgs.fetchFromGitHub {
            owner = "mokrates";
            repo = "json-simple-flymake";
            rev = "f3dacf070d1e04d5805323b0a95d58c5b9b7f607";
            sha256 = "sha256-bMF3bdPKL1kiEdijPNijQtEPh3RWXn15z080vmM/TBQ=";
          }; 
        };
      in utils.emacsPkg {
        name = "db-json";
        description = "json configuration";
        require = true;
        packages = [ json-simple-flymake ];
        code = ''
;; ~!emacs-lisp!~
;; Using json-ts-mode for json files
(add-to-list 'major-mode-remap-alist `(js-json-mode . json-ts-mode))

;; Activating json flymake checke upon enabling json-ts-mode
(add-hook 'json-ts-mode-hook
	  (lambda ()
	    ;; load json flymake checker
	    (load-library "json-simple-flymake")
	    (json-simple-setup-flymake-backend)

	    ;; Add flymake diagnostics to mode bar
	    (add-to-list 'mode-line-misc-info
			 `(flymake-mode (" " flymake-mode-line-counters " ")))
	    ;; Enable flymake
	    (flymake-mode t)))
'';
      };
    };
  };
}
