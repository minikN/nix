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
          wdisplays # GUI
        ];
      };

      ## Configure wdisplays windows to be floating
      wayland.windowManager.sway = lib.mkIf (config.os.wm == "sway") {
        extraConfig = let
          clamshell = pkgs.writeShellScriptBin "clamshell" ''
if grep -q open /proc/acpi/button/lid/LID1/state; then
    swaymsg output ${config.os.output.primary.name} enable
    ${pkgs.libnotify}/bin/notify-send -t 5000 "Enabled primary output"
else
    swaymsg output ${config.os.output.primary.name} disable
    ${pkgs.libnotify}/bin/notify-send -t 5000 "Disabled primary output"
fi
'';
        in ''
bindswitch --reload --locked lid:on output ${config.os.output.primary.name} disable
bindswitch --reload --locked lid:off output ${config.os.output.primary.name} enable
'';
        config.window.commands = [
          {
            command = "floating enable, border pixel 0";
            criteria = {
              app_id = "wdisplays";
            };
          }
        ];
      };
    };
  };
}
