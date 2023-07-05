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
### GnuPG configuration
###
### CODE:

{ config, lib, pkgs, ... }:

{
  home-manager.users.${config.user} = {
    services = {
     
     ## Enable gpg-agent with ssh support
     gpg-agent = {
      enable = true;
      enableSshSupport = true;
      enableZshIntegration = config.os.shell == "zsh";
      pinentryFlavor = "qt";
     };

     ## Add SSH key
     gpg-agent.sshKeys = [ "E3FFA5A1B444A4F099E594758008C1D8845EC7C0" ];
    };

    programs = {
      gpg = {
        
        ## Enable GnuPG
        enable = true;

        ## Set homedir to `$XDG_DATA_HOME/gnupg'. This will set GNUPG_HOME.
        homedir = "${config.home-manager.users.${config.user}.xdg.dataHome}/gnupg";
      };
    };
  };
}

