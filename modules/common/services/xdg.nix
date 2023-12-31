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
### XDG configuration
###
### CODE:

{ config, lib, pkgs, ... }:

{
  config = {

    ## Setting up portals
    xdg.portal = {
      enable = true;
      wlr.enable = config.os.wayland;
      configPackages = [ pkgs.xdg-desktop-portal-gtk ];
    };

    ## Needed so that the user dirs get exported as env vars
    environment.systemPackages = [ pkgs.xdg-user-dirs ];

    ## Enabling XDG
    home-manager.users.${config.user} = {
      xdg.enable = true;

      ## Setting custom cacheHome
      xdg.cacheHome = "${config.home-manager.users.${config.user}.home.homeDirectory}/.local/cache";
      
      ## Enable XDG mime type handling
      xdg.mime.enable = true;
      xdg.mimeApps.enable = true;
      
      ## Enable XDG user directories
      xdg.userDirs = {
        enable = true;
        createDirectories = true;
        documents = "${config.home-manager.users.${config.user}.home.homeDirectory}/Documents";
        download = "${config.home-manager.users.${config.user}.home.homeDirectory}/Downloads";
        pictures = "${config.home-manager.users.${config.user}.home.homeDirectory}/Pictures";
        
        ## Unused directories
        desktop = null;
        music = null;
        publicShare = null;
        templates = null;
        videos = null;
      };
    };
  };
}

