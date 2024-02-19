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
### Emacs embark configuration
###
### CODE:

{ config, lib, pkgs, inputs, ... }:

{
  config = let
    utils = import ./../../../../utils.nix { inherit lib pkgs config; };
  in {
    home-manager.users.${config.user} = {
      programs.emacs = utils.emacsPkg {
        name = "db-embark";
        description = "Embark configuration";
        require = true;
        packages = [
          pkgs.emacsPackages.embark
          pkgs.emacsPackages.embark-consult
        ];
        code = ''
;; ~!emacs-lisp!~
;; Nothing yet.
'';
        config = ''
;; ~!emacs-lisp!~
(use-package embark
  :bind ("s-." . embark-act)
  :init
  ;; embark window options
  ;; TODO: Actions still open in other window
  (add-to-list 'display-buffer-alist
	       '("\\*Embark Actions\\*"
		 (display-buffer-reuse-mode-window display-buffer-below-selected)
		 (window-height . fit-window-to-buffer)
		 (window-parameters . ((no-other-window . t)
                                       (mode-line-format . none)
				       (header-line-format . none)))))
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))
  
  ;; Addings embark completion categories to vertico
  (with-eval-after-load 'db-vertico
    (add-to-list 'vertico-multiform-categories
		 `(embark-keybinding ,@db-vertico-multiform-maximal))))

(use-package embark-consult
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))
'';
      };
    };
  };
}
