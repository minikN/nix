{
  description = "A very basic flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = inputs@{ nixpkgs, home-manager, ... }:
  let
    system = "x86_64-linux";
    
    pkgs = import nixpkgs {
      inherit system;
      config = { allowUnfree = true; };
    };
    
    mkMachine = machineConfig: user: modules: homeModules: inputs.nixpkgs.lib.nixosSystem {
      inherit system;
      modules = [
        machineConfig
        #(./. + "/users/${user}")
        home-manager.nixosModules.home-manager {
          home-manager.useGlobalPkgs = true;
          home-manager.useUserPackages = true;
	  home-manager.users.db.imports = homeModules;
        }
      ] ++ modules;
    };

  in {
    nixosConfigurations = {
      slimboy = mkMachine
        ./machines/slimboy.nix
        null
        []
        [];
    };
  };
}
