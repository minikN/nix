{
  description = "A very basic flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    home-manager.url = "github:nix-community/home-manager/master";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = {nixpkgs, ...}@inputs:
  let

    globals = rec {
      user = "db";
      fullName = "Demis Balbach";
    };

    supportedSystems = [ "x86_64-linux" ];
    forAllSystems = nixpkgs.lib.genAttrs supportedSystems;

  in rec {
    
    nixosConfigurations = {
      slimboy = import ./machines/slimboy.nix { inherit inputs globals; };
    };

    homeConfigurations = {
      slimboy = nixosConfigurations.slimboy.config.home-manager.users.${globals.user}.home;
    };
  };
}
