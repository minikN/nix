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
  system,
  ...
}:

{
  imports =
    [ ]
    ++ lib.optional (system == "x86_64-linux") ./linux.nix
    ++ lib.optional (system == "aarch64-darwin") ./darwin.nix;

  ## Global options
  ##
  ## These can be used throughout the configuration. If a value
  ## with the same name has been declared in `globals', its
  ## value will be set as default for the respective option.
  options =
    let
      mkConst = const: (lib.mkOption { default = const; });
    in
    {

      user = lib.mkOption {
        # is defined in flake.nix
        type = lib.types.str;
        description = "Primary user of the system";
      };

      fullName = lib.mkOption {
        # is defined in flake.nix
        type = lib.types.str;
        description = "Full name of the user";
      };

      email = lib.mkOption {
        # is defined in flake.nix
        type = lib.types.str;
        description = "Primary email of the user";
      };

      gpgKey = lib.mkOption {
        # is defined in flake.nix
        type = lib.types.str;
        description = "GPG key of the user";
      };

      stateVersion = lib.mkOption {
        # is defined in flake.nix
        type = lib.types.str;
        description = "State version of nixos and home-manager";
      };
    };

  ## Global configuration
  ##
  ## Should only contain global settings that are not related to
  ## any particular part of the system and could therefore be
  ## extracted into their own module.
  config = {
    nix = {
      ## Enabling flakes
      extraOptions = ''
        experimental-features = nix-command flakes
        warn-dirty = false
      '';
    };

    ## Timezone and locales
    time.timeZone = "Europe/Berlin";

    ## Allow unfree packages
    nixpkgs.config.allowUnfree = true;

    ## Global packages
    ##
    ## Packages should be managed with home-manager whereever
    ## possible. Only use a set of barebones applications here.
    environment.systemPackages = with pkgs; [
      git
      vim
      wget
      curl
      nixfmt
      nixd
    ];

    ordenada = {
      users = {
        ${config.user} = { };
      };
      features = {
        userInfo = {
          username = "${config.user}";
          fullName = "${config.fullName}";
          email = "${config.email}";
          gpgPrimaryKey = "${config.gpgKey}";
        };
        gnupg = {
          enable = true;
          sshKeys = [ "E3FFA5A1B444A4F099E594758008C1D8845EC7C0" ];
        };
        git = {
          enable = true;
          signCommits = true;
        };
      };
    };

    home-manager.users.${config.user} = {
      home = {
        packages = with pkgs; [ ];
      };
    };
  };
}
