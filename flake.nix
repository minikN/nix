{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    systems.url = "github:nix-systems/default";
    flake-parts.url = "github:hercules-ci/flake-parts";

    darwin = {
      url = "github:lnl7/nix-darwin/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    ordenada = {
      #url = "github:migalmoreno/ordenada";
      url = "git+file:./../ordenada";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        home-manager.follows = "home-manager";
      };
    };

  };

  outputs =
    inputs@{
      flake-parts,
      nixpkgs,
      ...
    }:
    let
      globals = {
        user = "db";
        fullName = "Demis Balbach";
        email = "db@minikn.xyz";
        gpgKey = "F17DDB98CC3C405C";
        stateVersion = "25.11";
      };
    in
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = import inputs.systems;
      imports = [ inputs.ordenada.flakeModule ];
      flake = rec {

        darwinConfigurations = {
          workhorse = import ./machines/workhorse.nix {
            inherit
              inputs
              globals
              nixpkgs
              ;
          };
        };

        nixosConfigurations = {
          slimboy = import ./machines/slimboy.nix {
            inherit
              inputs
              globals
              nixpkgs
              ;
          };
        };
        homeConfigurations = {
          slimboy = nixosConfigurations.slimboy.config.home-manager.users.${globals.user}.home;
          workhorse = darwinConfigurations.workhorse.config.home-manager.users.${globals.user}.home;
        };
      };
    };
}
