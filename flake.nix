{
  description = "My NixOS configuration";

  ## Inputs
  ##
  ## Using latest commits for both nixpkgs and home-manager
  ## to make NixOS rolling release.
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    home-manager.url = "github:nix-community/home-manager/master";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { nixpkgs, ... }@inputs:
  let

    ## Global variables used throughout the configuration
    globals = rec {
      user = "db";
      fullName = "Demis Balbach";
      stateVersion = "23.05";
    };

    supportedSystems = [ "x86_64-linux" ];
    forAllSystems = nixpkgs.lib.genAttrs supportedSystems;

  in rec {
    
    ## System configurations
    nixosConfigurations = {
      slimboy = import ./machines/slimboy.nix { inherit inputs globals nixpkgs; };
    };

    ## Home configurations
    homeConfigurations = {
      slimboy = nixosConfigurations.slimboy.config.home-manager.users.${globals.user}.home;
    };
  };
}
