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
### Networking configuration
###
### CODE:

{ config, lib, pkgs, ... }:

{
  ## DHCP needs to be enable on a per-interface basis.
  ## This is part of the machine-specific configuration.
  networking.useDHCP = false;

  ## Enable network manager
  networking.networkmanager.enable = true;

  ## Enabling appropriate groups
  users.users.${config.user} = {
    extraGroups = [ "networkmanager" ]; 
  };

  ## Configuring nm app windows to be floating
  home-manager.users.${config.user} = {
    home.packages = [ pkgs.networkmanagerapplet ];
    wayland.windowManager.sway = lib.mkIf (config.os.wm == "sway") {
      config.window = {
        commands = [
          {
            command = "floating enable, border pixel 2";
            criteria = {
              app_id = "nm-.*";
            };
          }
        ];
      };

      ## Enabling nm-applet via config.programs.nm-applet will not show icon
      ## Need to do it through sway
      extraConfig = ''
        exec nm-applet --indicator
      '';
    };
    
  };
}
