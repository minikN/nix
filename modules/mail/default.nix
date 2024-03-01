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
      clients = {
        thunderbird = {
          enable = lib.mkEnableOption "test";
        };
        emacs = {
          enable = lib.mkEnableOption "test";
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
      systemd.user.services.imapnotify-primary.Service.ExecStart =
        lib.mkForce "${lib.getExe config.home-manager.users.${config.user}.services.imapnotify.package} -debug -conf '${config.home-manager.users.${config.user}.xdg.configHome}/imapnotify/imapnotify-primary-config.json'";
      
      accounts.email.maildirBasePath = config.const.mailDir;

      services.imapnotify.enable = true;
      programs.mbsync.enable = true;
      programs.msmtp.enable = true;
      
      services.mbsync = {
        enable = true;
        preExec = toString (pkgs.writeShellScript "mbsync-pre" ''
## ~!shell!~
mkdir -p ${config.const.mailDir}
for f in $(ls ${config.const.mailDir}/accounts); do
    mkdir -p ${config.const.mailDir}/$f
done
          '');
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
