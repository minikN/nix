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

    # Repositories
    ../repositories/dotfiles.nix

    # WM / GUI
    ./wm/sway.nix
    ./wm/cursor.nix
    ./wm/launcher/rofi.nix
    ./wm/bar/waybar.nix
    ./image/screenshot/swappy.nix

    ## Scripts
    ./scripts/screenshot.nix

    ## Terminal
    ./terminal/alacritty.nix
    ./shell/zsh.nix

    ## File manager
    ./file-manager/thunar.nix

    ## Services
    ./services
    ./services/xdg.nix
    ./services/pipewire.nix
    ./services/bluetooth.nix
    ./services/printer.nix
    ./services/flatpak.nix

    ## Security
    ./security/gpg.nix
    ./security/pass.nix
    ./security/tessen.nix

    ## System
    ./system/boot.nix
    ./system/filesystem.nix
    ./system/networking.nix
    ./system/fonts.nix

    ## Hardware
    ../hardware/color-temperature.nix
    ../hardware/outputs.nix

    ## Emacs
    ../development/emacs

    ## Mail
    ../mail

  ];

  
  ## Global options
  ##
  ## These can be used throughout the configuration. If a value
  ## with the same name has been declared in `globals', its
  ## value will be set as default for the respective option.
  options = let
    mkConst = const: (lib.mkOption { default = const; });
   in {

    user = lib.mkOption { # is defined in flake.nix
      type = lib.types.str;
      description = "Primary user of the system";
    };

    fullName = lib.mkOption { # is defined in flake.nix
      type = lib.types.str;
      description = "Full name of the user";
    };
    
    stateVersion = lib.mkOption { # is defined in flake.nix
      type = lib.types.str;
      description = "State version of nixos and home-manager";
    };

    ## Constants
    ##
    ## Object of options that can be set throughout the configuration.
    ## Meant for options that get set by any module once, and never again.
    const = {
      signingKey = mkConst "F17DDB98CC3C405C";
      passDir = mkConst "${config.users.users.${config.user}.home}/.local/var/lib/password-store";
      mailDir = mkConst "${config.users.users.${config.user}.home}/.local/var/lib/mail";
    };

    ## Namespacing some options so they don't interfere with
    ## other nix options.
    os = {
      keyboard = {
        layout = lib.mkOption {
          type = lib.types.str;
          description = "Primary keyboard layout";
          default = "us";
        };

        options = lib.mkOption {
          type = lib.types.commas;
          description = "Keyboard options";
          default = ''
            ctrl:nocaps
          '';
        };
      };

      wayland = lib.mkOption {
        type = lib.types.bool;
        default = false;
        description = "Whether wayland is used on the system";
      };

      passwordManager = lib.mkOption {
        type = lib.types.path;
        description = "The password manager in use";
      };

      machine = {
        isLaptop = lib.mkOption {
          type = lib.types.bool;
          description = "Whether the machine is a laptop";
          default = false;
        };

        temperaturePath = lib.mkOption {
          type = lib.types.path;
          description = "Machine specific path to the core temp class";
          default = "/sys/class/hwmon/hwmon4/temp1_input";
        };
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

      ## Store optimization
      optimise.automatic = true;

      ## Automatic garbage collection
      gc = {
        automatic = true;
        dates = "weekly";
        options = "--delete-older-than 7d";
      };
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

    ## Allow unfree packages
    nixpkgs.config.allowUnfree = true;

    ## Setting correct application settings if we're running wayland
    environment.sessionVariables = lib.mkIf config.os.wayland {
      NIXOS_OZONE_WL = "1";
      GTK_USE_PORTAL = "1";
    };
	
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

      home = lib.mkMerge [
        {
          ## Setting state version for home-manager
          stateVersion = "${config.stateVersion}";

          ## Global home packages
          packages = with pkgs; [
              libnotify
              unzip
              mediaelch
              kid3 # mp3 tag editor
              mpv
              realvnc-vnc-viewer
              gnome.gnome-system-monitor
              gnome.file-roller
              gpgme
            ];
        }
        (lib.mkIf config.os.wayland {

          ## Wayland specific packages
          packages = with pkgs; [
            wl-clipboard
            grim
            slurp
            mako
          ];
        })
      ];
    };


    ## Setting state version for system
    system.stateVersion = "${config.stateVersion}";
  };
}

