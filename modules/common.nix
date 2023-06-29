{ config, lib, pkgs, ... }:

{
  options = {
    user = lib.mkOption {
      type = lib.types.str;
      description = "Primary user of the system";
    };

    fullName = lib.mkOption {
      type = lib.types.str;
      description = "Full name of the user";
    };
    
    stateVersion = lib.mkOption {
      type = lib.types.str;
      description = "State version of nixos and home-manager";
    };
  };

  config = {
    nix = {
      extraOptions = ''
        experimental-features = nix-command flakes
        warn-dirty = false
      '';
    };

    environment.systemPackages = with pkgs; [ git vim wget curl ];

    home-manager.useGlobalPkgs = true;
    home-manager.useUserPackages = true;
    home-manager.users.${config.user}.home.stateVersion = "${config.stateVersion}";
  
    system.stateVersion = "${config.stateVersion}";
  };
}

