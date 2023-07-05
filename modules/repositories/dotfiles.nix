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
### dotfiles repository
###
### CODE:

{ config, lib, pkgs, ... }:

{
  imports = [
    ## Import the configuration so we know where to clone
    ../repositories
  ];

  options = {
    repositories = {
      dotfiles = {
        path = lib.mkOption {
          type = lib.types.path;
          description = "Path where the dotfiles should be cloned";
          default = "${config.repositories.path}/dots";
        };

        repo = lib.mkOption {
          type = lib.types.str;
          description = "URL of the dotfiles repo";
          default = "git@github.com:minikN/nix.git";
        };
      };
    };
  };

  config = {
    home-manager.users.${config.user} = {
      home.activation = {
        cloneDotfiles =
            ## Source: https://github.com/nmasur/dotfiles/blob/cd0c93c6d9a7dfa5ed061a850140f7f4f8bc9323/modules/common/repositories/dotfiles.nix
            config.home-manager.users.${config.user}.lib.dag.entryAfter
            [ "writeBoundary" ] ''
              if [ ! -d "${config.repositories.dotfiles.path}" ]; then
                  $DRY_RUN_CMD mkdir --parents $VERBOSE_ARG $(dirname "${config.repositories.dotfiles.path}")

                  # Force HTTPS because anonymous SSH doesn't work
                  GIT_CONFIG_COUNT=1 \
                      GIT_CONFIG_KEY_0="url.https://github.com/.insteadOf" \
                      GIT_CONFIG_VALUE_0="git@github.com:" \
                      $DRY_RUN_CMD \
                      ${pkgs.git}/bin/git clone ${config.repositories.dotfiles.repo} "${config.repositories.dotfiles.path}"
              fi
            '';
      };
    };
  };
}

