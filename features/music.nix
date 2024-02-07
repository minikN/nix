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
### Enables modules commonly used for music
###
### CODE:

{ config, lib, pkgs, ... }:

{
  imports = [
    ## Reverse import the wrapper feature so that the options are defined.
    ../features
  ];

  config = let
    amp-locker-icon =  builtins.fetchurl {
      url = "https://audioassault.mx/cdn/shop/files/favicon_32x32.jpg";
      sha256 = "04g37k9blsg7zj553ma1ssijpgih0ldpwnbfxjqw2d7kiksdjii4";
    };
  in {

    features.music = true;


    nixpkgs.overlays = [
      (self: super: {
        amp-locker = super.callPackage ../packages/audio/amp-locker.nix { };
        amp-locker-desktop = super.writeTextDir "share/applications/amp-locker.desktop" ''
          [Desktop Entry]
          Version=${pkgs.amp-locker.version}
          Type=Application
          Name=Amp Locker
          Icon=${toString amp-locker-icon}
          Exec=${pkgs.amp-locker}/bin/amp-locker
        '';
        audioassault-blacksun = super.callPackage ../packages/audio/audioassault-blacksun.nix { };
        audioassault-blacksun-desktop = super.writeTextDir "share/applications/audioassault-blacksun.desktop" ''
          [Desktop Entry]
          Version=${pkgs.audioassault-blacksun.version}
          Type=Application
          Name=Audio Assault Blacksun
          Icon=${toString amp-locker-icon}
          Exec=${pkgs.audioassault-blacksun}/bin/audioassault-blacksun
        '';
      })
    ];

    # enabling musnix
    musnix = {
     enable = true;

      # Setting kernel related parameters
      kernel = {
        realtime = true;
        packages = pkgs.linuxPackages_latest_rt;
      };
    };

    # Enabling the jack service
    services.jack = {
      jackd.enable = true;
      alsa.enable = true;
    };

    users.users.${config.user}.extraGroups = [ "jackaudio" "pro-audio" "vst-plugins" ];

    home-manager.users.${config.user}.home = {
      packages = with pkgs; [
        ardour
        carla

        # guitar plugins
        amp-locker
        amp-locker-desktop
        audioassault-blacksun
        audioassault-blacksun-desktop

        # jack-related
        qjackctl
        jack2Full
        jack_capture
      ];

      # Copying guitar plugins to their respective plugin dir, so other apps can pick them up
      # ~/.vst, ~/.vst3
      file.".vst/Amp Locker vst2.so".source = "${pkgs.amp-locker}/lib/vst/Amp Locker vst2.so";
      file.".vst3/Amp Locker.vst3".source = "${pkgs.amp-locker}/lib/vst3/Amp Locker.vst3";
      file.".vst/Blacksunvst2.so".source = "${pkgs.audioassault-blacksun}/lib/vst/Blacksunvst2.so";
    };
  };
}
