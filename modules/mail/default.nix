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
  options = {
    mail = {
     address = lib.mkOption {
        type = lib.types.str;
        default = "";
      };

      signature = lib.mkOption {
        type = lib.types.lines;
        description = "Primary signature";
        default = '''';
      };
      
      clients = {
        thunderbird = {
          enable = lib.mkEnableOption "Enable thunderbird as a mail client";
          primary = lib.mkOption {default = ""; };
        };
        emacs = {
          enable = lib.mkEnableOption "Enable emacs as a mail client";
          primary = lib.mkOption {default = ""; };
        };
      };
    };
  };
  
  imports = [
    ./mailbox.nix
    ./work.nix

    ./clients/emacs.nix
    ./clients/thunderbird.nix
  ];

  ## General mail settings
  config = {
    home-manager.users.${config.user} = {
      accounts.email.maildirBasePath = config.const.mailDir;
      services.imapnotify.enable = true;
      programs.mbsync.enable = true;
      programs.msmtp.enable = true;
      
      services.mbsync = {
        enable = true;
        postExec = "${pkgs.notmuch}/bin/notmuch new";
      };
      
      programs.notmuch = {
        enable = true;
        new = {
          tags = [ "new" ];
          ignore = [
            ".uidvalidity"
            ".mbsyncstate"
            ".mbsyncstate.new"
            ".mbsyncstate.journal"
          ];
        };
        
        search.excludeTags = [ "trash"  "spam"  "deleted" ];
      };
    };
  };
}
