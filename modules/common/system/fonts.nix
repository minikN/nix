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
### Fonts configuration
###
### CODE:

{ config, lib, pkgs, ... }:

{
  options = {
    os.fonts = {
      size = lib.mkOption {
        type = lib.types.int;
        default = 16;
      };

      mono = {
        regular = lib.mkOption {
          type = lib.types.str;
          description = "Default monospaced font";
          default = "Iosevka Nerd Font";
        };

        light = lib.mkOption {
          type = lib.types.str;
          description = "Default monospaced font";
          default = "${config.os.fonts.mono.regular} Light";
        };
      };
      
      sans = {
        regular = lib.mkOption {
          type = lib.types.str;
          description = "Default sans font";
          default = "Iosevka Aile";
        };
      };
      
      sans-serif = {
        regular = lib.mkOption {
          type = lib.types.str;
          description = "Default sans-serif font";
          default = "Iosevka Etoile";
        };
      };

      emoji = {
        regular = lib.mkOption {
          type = lib.types.str;
          description = "Default emoji font";
          default = "Noto Color Emoji";
        };
      };
    };
  };

  config = {
    fonts = {
      fontconfig = {
        enable = true;
        defaultFonts = {
          monospace = [ "Iosevka Nerd Font" ];
          serif = [ "Iosevka Etoile" ];
          sansSerif = [ "Iosevka Aile" ];
        };
      };
      packages = with pkgs; [
        (nerdfonts.override { fonts = [ "Iosevka" ]; })
        (iosevka-bin.override { variant = "aile"; })
        (iosevka-bin.override { variant = "etoile"; })
        noto-fonts-emoji
      ];
    };
  };
}

