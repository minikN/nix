# ## NixOS Configuration
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
### Common configuration
### Should probably be used on all machines.
###
### CODE:

{
  config,
  lib,
  pkgs,
  ordanada,
  ...
}:

{
  ## Global configuration
  ##
  ## Should only contain global settings that are not related to
  ## any particular part of the system and could therefore be
  ## extracted into their own module.
  config = {
    nix = {
      ## TODO: Create option for determinate
      ## builtins.pathExists /nix/var/nix/profiles/default/etc/profile.d/nix-installer-version
      ## is only true if determinate is used
      enable = false;

      ## Automatic garbage collection
      gc = {
        interval = {
          Hour = 3;
          Minute = 15;
          Weekday = 7;
        };
        options = "--delete-older-than 7d";
      };
    };

    ## Allow unfree packages
    nixpkgs.config.allowUnfree = true;

    ## Global packages
    ##
    ## Packages should be managed with home-manager whereever
    ## possible. Only use a set of barebones applications here.
    #environment.systemPackages = with pkgs; [ git vim wget curl ];

    ordenada = {
      features = {
        home.enable = true;
      };
    };

    home-manager.users.${config.user} = {
      home = {
        packages = with pkgs; [ ];
      };
    };

    ## Setting state version for system
    system.stateVersion = 6;
  };
}
