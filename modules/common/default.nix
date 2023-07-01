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


{ config, lib, pkgs, ... }:

{
  imports = [
    ./git.nix
    ./gpg.nix
    ./shell/zsh.nix

    # WM / GUI
    ./wm/sway.nix
    ./wm/cursor.nix
    ./terminal/alacritty.nix

    ## services
    ./services
    ./services/xdg.nix

    ## system
    ./system/boot.nix
    ./system/filesystem.nix
    ./system/networking.nix
    ./system/fonts.nix
  ];
  
  ## Global options
  ##
  ## These can be used throughout the configuration. If a value
  ## with the same name has been declared in `globals', its
  ## value will be set as default for the respective option.
  options = {
    user = lib.mkOption {
      type = lib.types.str;
      description = "Primary user of the system";
    };

    fullName = lib.mkOption {
      type = lib.types.str;
      description = "Full name of the user";
    };

    primaryMail = lib.mkOption {
      type = lib.types.str;
      description = "Primary email";
      default = "db@minikn.xyz";
    };

    stateVersion = lib.mkOption {
      type = lib.types.str;
      description = "State version of nixos and home-manager";
    };

    ## Namespacing some options so they don't interfere with
    ## other nix options.
    os = {
      layout = lib.mkOption {
        type = lib.types.str;
        description = "Primary keyboard layout";
        default = "us";
      };

      wm = lib.mkOption {
        type = lib.types.str;
        description = "Window manager used throughout the system";
      };

      wayland = lib.mkOption {
        type = lib.types.bool;
        default = false;
        description = "Whether wayland is used on the system";
      };

      terminal = lib.mkOption {
        type = lib.types.path;
        default = if config.os.wayland
          then "${pkgs.foot}/bin/foot"
          else "${pkgs.gnome.gnome-terminal}/bin/gnome-terminal";
      };
    };
  };

  ## Global configuration
  ##
  ## Should only contain global settings that are not related to
  ## any particular part of the system and could therefore be
  ## extracted into their own module.
  config = {
    nix = {

      ## Enabling flakes
      extraOptions = ''
        experimental-features = nix-command flakes
        warn-dirty = false
      '';
    };

    ## OpenGL support
    hardware.opengl = {
      enable = true;
      driSupport = true;
      driSupport32Bit = true;
    };

    ## Timezone and locales
    ##
    ## I don't travel
    time.timeZone = "Europe/Berlin";
    i18n.defaultLocale = "en_US.UTF-8";

    ## Global packages
    ##
    ## Packages should be managed with home-manager whereever
    ## possible. Only use a set of barebones applications here.
    environment.systemPackages = with pkgs; [ git vim wget curl ];

    ## Home manager settings
    home-manager.useGlobalPkgs = true;
    home-manager.useUserPackages = true;

    ## Setting the `stateVersion' for both home-manager and system.
    home-manager.users.${config.user} = {
      
      ## Setting state version for home-manager
      home.stateVersion = "${config.stateVersion}";
    };

    ## Setting state version for system
    system.stateVersion = "${config.stateVersion}";
  };
}

