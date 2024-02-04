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
### Features configuration
###
### CODE:

{ config, lib, pkgs, ... }:

{
  ## Setting the appropriate option so other modules know it
  options = {
    features = {
      development = lib.mkOption {
        type = lib.types.bool;
        description = "Whether the development feature is enabled";
        default = false;
      };
      
      gaming = lib.mkOption {
        type = lib.types.bool;
        description = "Whether the gaming feature is enabled";
        default = false;
      };

      music = lib.mkOption {
        type = lib.types.bool;
        description = "Whether the music feature is enabled";
        default = false;
      };

      
      "3d-printing" = lib.mkOption {
        type = lib.types.bool;
        description = "Whether the 3d-printing feature is enabled";
        default = false;
      };
    };
  };
}
