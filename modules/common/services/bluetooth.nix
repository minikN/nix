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
### Pipewire configuration
###
### CODE:

{ config, lib, pkgs, ... }:

{
  config = {

    ## Enabling bluetooth
    hardware.bluetooth.enable = true;
    services.blueman.enable = true;

    ## Enabling tray icon
    home-manager.users.${config.user} = {
      services.blueman-applet.enable = true;

      ## Configure blueman windows to be floating
      wayland.windowManager.sway.config.window = {
        commands = lib.mkIf (config.os.wm == "sway") [
          {
            command = "floating enable, border pixel 2";
            criteria = {
              app_id = "\.blueman.*";
            };
          }
        ];
      };
    };
  };
}

