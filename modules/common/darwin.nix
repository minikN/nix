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

    ## Global packages
    environment.systemPackages = with pkgs; [ ];

    ## Ordenada
    ordenada = {
      features = {
        home.enable = true;
        homebrew.enable = true;
        android = {
          enable = true;
          emulators = [
            {
              name = "smallPhone";
              platformVersion = "35";
              abiVersion = "arm64-v8a";
            }
          ];
          activeSdk = "35";
          sdks = [
            {
              identifier = "35";
              packages = [
                "platform-tools"
                "platforms;android-35"
                "build-tools;35.0.0"
                "cmdline-tools;latest"
                "system-images;android-35;google_apis_playstore;arm64-v8a"
              ];
            }
          ];
        };
        ios = {
          enable = true;
          xcodeVersions = [ "16.4" ];
          xcodeActiveVersion = "16.4";
          simulators = [
            {
              device = "iPhone-16-Pro";
              os = "iOS 18.5";
              bootOnLogin = true;
            }
          ];
        };
        aerospace = {
          enable = true;
          up = "i";
          left = "j";
          down = "k";
          right = "l";
        };
      };
    };

    ## User packages
    home-manager.users.${config.user} = {
      home.packages = with pkgs; [
        jetbrains.idea-ultimate
      ];
    };

    ## Setting state version for system
    system.stateVersion = 6;
  };
}
