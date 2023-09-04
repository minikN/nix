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
### Outputs configuration
### Static list of several output configurations used on different machines
###
### CODE:

{ config, lib, pkgs, ... }:

{
  options = {
    os = {
      output = {
        primary = {
          name = lib.mkOption {
            type = lib.types.str;
            description = "Primary output of the machine";
            default = "eDP-1";
          };
          hidpi = lib.mkOption {
            type = lib.types.bool;
            description = "Whether the primary output is a HiDPI display";
          };
        };
        
        left = lib.mkOption {
          type = lib.types.listOf lib.types.str;
          description = "List of attribute sets which each describe a output that is considered `left'";
        };

        right = lib.mkOption {
          type = lib.types.listOf lib.types.str;
          description = "List of attribute sets which each describe a output that is considered `right'";
        };
      };
    };
  };

  config = {
    home-manager.users.${config.user}.services.kanshi = {
      enable = true;
      profiles = {
        home = {
          outputs = [
            { criteria = "DP-1"; }
            { criteria = "DP-2"; }
          ];
        };
        work = {
          outputs = [
            { criteria = "HDMI-A-1"; }
            { criteria = "HDMI-A-2"; }
          ];
        };
      };
    };
  };
}