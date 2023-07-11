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
### Mail configuration
###
### CODE:

{ config, lib, pkgs, ... }:

{
  imports = [
    ./mailbox.nix
    ./work.nix
    ../development/emacs/mail.nix
    ];

  ## General mail settings
  config = {
    home-manager.users.${config.user} = {
        services = {
          #mbsync = {
          #  enable = true;
          #  frequency = "*:0/10"; ## Every 1 minutes
          #  postExec = "${pkgs.notmuch}/bin/notmuch new";
          #};
          imapnotify = {
            enable = true;
          };
        };
        programs.mbsync.enable = true;
        programs.msmtp.enable = true;
        programs.astroid.enable = true;
        programs.notmuch = {
          enable = true;
        };
    };
  };
}