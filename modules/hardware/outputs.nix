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
    home-manager.users.${config.user} = {
      home = lib.mkIf (config.os.wayland == true) {
        packages = with pkgs; [
          way-displays
          wdisplays # GUI
        ];
      };
      xdg.configFile."way-displays/cfg.yaml".text = ''
## ~!yaml!~
ARRANGE: ROW
ALIGN: BOTTOM
ORDER:
${lib.lists.foldl (all: output:
  if builtins.stringLength all > 0
  then "${all}\n  - '${output.left.id}'"
  else "  - '${output.left.id}'"
) "" config.os.output.configs}
  - '${config.os.output.primary.name}'
${lib.lists.foldl (all: output:
  if builtins.stringLength all > 0
  then "${all}\n  - '${output.right.id}'"
  else "  - '${output.right.id}'"
) "" config.os.output.configs}
SCALING: FALSE
AUTO_SCALE: FALSE
'';

      ## Configure wdisplays windows to be floating
      wayland.windowManager.sway.config.window = {
        commands = lib.mkIf (config.os.wm == "sway") [
          {
            command = "floating enable, border pixel 0";
            criteria = {
              app_id = "wdisplays";
            };
          }
        ];
      };

      wayland.windowManager.sway.extraConfig = ''
exec ${pkgs.way-displays}/bin/way-displays > /tmp/way-displays.$XDG_VTNR.$USER.log 2>&1
'';
    };
  };
}
