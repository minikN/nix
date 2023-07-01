##
## Common configuration
##
## Should probably be used on all machines.
##

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

