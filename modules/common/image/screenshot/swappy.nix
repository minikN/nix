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
### Swappy configuration
###
### CODE:

{ config, lib, pkgs, ... }:

{
  config = {
    home-manager.users.${config.user} = {
      home.packages = [ pkgs.swappy ];

      ## Writing swappys config file
      xdg.configFile."swappy/config".text = ''
        [Default]
        save_dir=${config.home-manager.users.${config.user}.xdg.userDirs.pictures}/screenshots
        save_filename_format=screenshot-%Y%m%d-%H%M%S.png
        show_panel=false
        line_size=5
        text_size=20
        text_font=sans-serif
        paint_mode=brush
        early_exit=false
        fill_shape=false
      '';
    };
  };
}

