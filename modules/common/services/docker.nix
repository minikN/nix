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
### Docker configuration
###
### CODE:

{ config, lib, pkgs, ... }:

{
  config = let
    utils = import ./../../../utils.nix { inherit lib pkgs config; };
  in {

    virtualisation.docker = {
      enable = true;
      storageDriver = "btrfs";
    };
    
    users.users.${config.user}.extraGroups = [ "docker" ];

    home-manager.users.${config.user}.programs.emacs = utils.emacsPkg {
      name = "db-docker";
      description = "Docker configuration";
      require = true;
      packages = [ pkgs.emacsPackages.docker ];
      code = ''
;; ~!emacs-lisp!~
(define-key mode-specific-map (kbd "d") '("Docker" . docker))
(with-eval-after-load 'docker
  (setq docker-compose-command "docker compose"))
'';
    };
  };
}

