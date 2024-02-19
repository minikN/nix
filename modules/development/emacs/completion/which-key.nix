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
### Emacs which-key configuration
###
### CODE:


{ config, lib, pkgs, inputs, ... }:

{
  config = let
    utils = import ./../../../../utils.nix { inherit lib pkgs config; };
  in {
    home-manager.users.${config.user} = {
      programs.emacs = utils.emacsPkg {
          name = "db-which-key";
          description = "which-key configuration";
          require = true;
          code = ''
;; ~!emacs-lisp!~
(defgroup db-which-key nil
  "Configuration related to `which-key'."
  :group 'db)

(defcustom db-which-key-min-lines 5
  "The minimum amount of lines which-key should display."
  :type 'number
  :group 'db-which-key)

(defcustom db-which-key-ellipsis "..."
  "The kind of ellipsis to use."
  :type 'string
  :group 'db-which-key)

(defcustom db-which-key-idle-delay 0.5
  "The delay before showing the popup."
  :type 'number
  :group 'db-which-key)
'';
          config = ''
;; ~!emacs-lisp!~
(use-package which-key
  :init
  (setq which-key-min-display-lines db-which-key-min-lines)
  ;; … takes the space of two characters, which missaligns some popups
  (setq which-key-ellipsis db-which-key-ellipsis)
  (setq which-key-idle-delay db-which-key-idle-delay)
  (which-key-mode 1))

(use-package which-key-posframe
  :init
  (setq which-key-posframe-parameters `((width . ,(frame-width))))
  :config
  (setq which-key-posframe-poshandler 'posframe-poshandler-frame-bottom-center)
  (which-key-posframe-mode))
'';
          packages = [ pkgs.emacsPackages.which-key pkgs.emacsPackages.which-key-posframe ];
        };
    };
  };
}


