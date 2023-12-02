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
  config = {
    home-manager.users.${config.user} = {
      programs.emacs = {
        extraPackages = epkgs: [ epkgs.which-key ];
        extraConfig = ''
          (require 'which-key)

          (defgroup ${config.user}-which-key nil
            "Configuration related to `which-key'."
            :group '${config.user})

          (defcustom ${config.user}-which-key-min-lines 5
            "The minimum amount of lines which-key should display."
            :type 'number
            :group '${config.user}-which-key)
          
          (defcustom ${config.user}-which-key-ellipsis "..."
            "The kind of ellipsis to use."
            :type 'string
            :group '${config.user}-which-key)
          
          (defcustom ${config.user}-which-key-idle-delay 0.5
            "The delay before showing the popup."
            :type 'number
            :group '${config.user}-which-key)

          (setq which-key-min-display-lines ${config.user}-which-key-min-lines)
          ;; … takes the space of two characters, which missaligns some popups
          (setq which-key-ellipsis ${config.user}-which-key-ellipsis)
          (setq which-key-idle-delay ${config.user}-which-key-idle-delay)

          (which-key-mode 1)
          
          (define-key global-map (kbd "C-h C-k") 'which-key-show-top-level)
        '';
      };
    };
  };
}


