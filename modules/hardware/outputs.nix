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
          width = lib.mkOption {
            type = lib.types.int;
            description = "Width of the primary output";
            default = 2560;
          };
          height = lib.mkOption {
            type = lib.types.int;
            description = "Height of the primary output";
            default = 1440;
          };
          hidpi = lib.mkOption {
            type = lib.types.bool;
            description = "Whether the primary output is a HiDPI display";
          };
        };
        
        configs = lib.mkOption {
          type = lib.types.listOf lib.types.attrs;
          description = "List of output configurations available for the current machine";
        };
      };
    };
  };

  config = {

    ## Forcing kanshi service to restart in order to correctly match new profile
    services.acpid.handlers.outputEvent.event = "jack/videoout.*";
    services.acpid.handlers.outputEvent.action = "systemctl --machine ${config.user}@.host --user restart kanshi.service";

    ## Kanshi configuration
    home-manager.users.${config.user}.services.kanshi = {
      enable = true;
      profiles = lib.lists.foldl (all: output: lib.attrsets.mergeAttrsList [
        all
        {
          "${output.name}-clamshell" = {
            outputs = [
              { criteria = "${output.left.id}"; position = "0,0"; }
              { criteria = "${output.right.id}"; position = "${toString output.left.width},0"; }
            ];
          };
          "${output.name}-open" = lib.mkIf (config.os.output.primary != null) {
            outputs = [
              { criteria = "${config.os.output.primary.name}"; position = "${toString output.left.width},0"; }
              { criteria = "${output.left.id}"; position = "0,0"; }
              { criteria = "${output.right.id}"; position = "${toString (output.left.width + config.os.output.primary.width)},0"; }
            ];
          };
        }
      ]) {} config.os.output.configs;
    };
  };
}
