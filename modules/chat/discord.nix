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
### Discord configuration
###
### CODE:

{ config, lib, pkgs, ... }:

{
  config = let
    ## Choosing the correct package in regards to the window system
    discord = if (config.os.wayland)

    ## Wrapping the discord package to enable wayland-specific features
    then pkgs.discord.overrideAttrs (old: {
        installPhase = old.installPhase + ''
          rm $out/bin/Discord
          rm $out/bin/discord

          makeWrapper $out/opt/Discord/Discord $out/bin/Discord \
            --prefix XDG_DATA_DIRS : $GSETTINGS_SCHEMAS_PATH \
            --prefix PATH : ${lib.makeBinPath [pkgs.xdg-utils]} \
            --add-flags "--ozone-platform=wayland --enable-features=UseOzonePlatform,WebRTCPipeWireCapturer"
          
          makeWrapper $out/opt/Discord/Discord $out/bin/discord \
            --prefix XDG_DATA_DIRS : $GSETTINGS_SCHEMAS_PATH \
            --prefix PATH : ${lib.makeBinPath [pkgs.xdg-utils]} \
            --add-flags "--ozone-platform=wayland --enable-features=UseOzonePlatform,WebRTCPipeWireCapturer"
        '';
      })
    else pkgs.discord;
  in {
    home-manager.users.${config.user}.home.packages = [ discord ];
  };
}

