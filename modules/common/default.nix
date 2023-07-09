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

    ## Security
    ./security/gpg.nix
    ./security/pass.nix
    ./security/tessen.nix

    ## System
    ./system/boot.nix
    ./system/filesystem.nix
    ./system/networking.nix
    ./system/fonts.nix

    ## Emacs
    ../development/emacs

    ## Mail
    ../mail/mailbox.nix
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
    
    signingKey = lib.mkOption {
      type = lib.types.str;
      description = "Primary key to use for signing";
    };

    primaryMail = lib.mkOption {
      type = lib.types.str;
      description = "Primary email";
      default = "db@minikn.xyz";
    };
    
    passDir = lib.mkOption {
      type = lib.types.path;
      description = "Default path to password-store";
      default = "${config.users.users.${config.user}.home}/.local/var/lib/password-store";
    };

    stateVersion = lib.mkOption {
      type = lib.types.str;
      description = "State version of nixos and home-manager";
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

      wm = lib.mkOption {
        type = lib.types.str;
        description = "Window manager used throughout the system";
      };

      launcher = {
        pkg = lib.mkOption {
          type = lib.types.path;
          description = "The launcher in use";
        };

        name = lib.mkOption {
          type = lib.types.str;
          description = "Name of the launcher used";
        };
        
        configFile = lib.mkOption {
          type = lib.types.path;
          description = "Config file of the current launcher";
        }; 

        args = lib.mkOption {
          type = lib.types.str;
          description = "Additional args for the launcher";
        };
      };

      bar = lib.mkOption {
        type = lib.types.path;
        description = "The bar used throughout the system";
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

      shell = lib.mkOption {
        type = lib.types.str;
        default = "bash";
        description = "Shell used on the system";
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

      ## Machine-specific outputs
      output = {
        primary = {
          name = lib.mkOption {
            type = lib.types.str;
            description = "Primary output of the machine";
          };
          hidpi = lib.mkOption {
            type = lib.types.bool;
            description = "Whether the primary output is a HiDPI display";
          };
        };
        
        left = {
          name = lib.mkOption {
            type = lib.types.str;
            description = "Left output of the machine";
          };
          hidpi = lib.mkOption {
            type = lib.types.bool;
            description = "Whether the left output is a HiDPI display";
          };
        };

        right = {
          name = lib.mkOption {
            type = lib.types.str;
            description = "Right output of the machine";
          };
          hidpi = lib.mkOption {
            type = lib.types.bool;
            description = "Whether the right output is a HiDPI display";
          };
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
    };

    ## OpenGL support
    hardware.opengl = {
      enable = true;
      driSupport = true;
      driSupport32Bit = true;
      # extraPackages = with pkgs; [
      #   intel-media-driver # LIBVA_DRIVER_NAME=iHD
      #   vaapiIntel         # LIBVA_DRIVER_NAME=i965 (older but works better for Firefox/Chromium)
      #   vaapiVdpau
      #   libvdpau-va-gl
      # ];
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

