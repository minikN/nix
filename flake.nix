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
### CODE:

{
  description = "My NixOS configuration";

  ## Inputs
  ##
  ## Using latest commits for both nixpkgs and home-manager
  ## to make NixOS rolling release.
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    nixos-hardware.url = "github:NixOS/nixos-hardware/master";
    ordenada.url = "github:migalmoreno/ordenada";
    #ordenada.url = "git+file:///home/db/ordenada";

    home-manager.url = "github:nix-community/home-manager/master";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    
    #musnix  = { url = "github:musnix/musnix"; };
    #audio.url = "github:polygon/audio.nix";
    #nur.url = "github:nix-community/NUR";
    #tuxedo-nixos = {
    #  url = "github:blitz/tuxedo-nixos";
    #};
    #emacs-overlay = {
    #  url = "github:nix-community/emacs-overlay";
    #  inputs.nixpkgs.follows = "nixpkgs";
    #};
  };

  outputs = { nixpkgs, nixos-hardware, ordenada, ... }@inputs:
  let

    ## Global variables used throughout the configuration
    globals = rec {
      user = "db";
      fullName = "Demis Balbach";
      stateVersion = "24.11";
    };

    overlays = [
      #inputs.nur.overlays.default
      #inputs.emacs-overlay.overlay
      #inputs.audio.overlays.default
    ];

    supportedSystems = [ "x86_64-linux" ];
    forAllSystems = nixpkgs.lib.genAttrs supportedSystems;

  in rec {

    ## System configurations
    nixosConfigurations = {
      slimboy = import ./machines/slimboy.nix { inherit inputs globals nixpkgs nixos-hardware ordenada overlays; };
    };

    ## Home configurations
   homeConfigurations = {
      slimboy = nixosConfigurations.slimboy.config.home-manager.users.${globals.user}.home;
   };
  };
}
