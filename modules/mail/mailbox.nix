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
### Mailbox configuration
###
### CODE:

{ config, lib, pkgs, ... }:

{
  ## Importing general mail settings
  imports = [ ../mail ];

  ## General mail settings
  config = {
     home-manager.users.${config.user}.accounts.email = {
      accounts.mailbox = {
        address = config.primaryMail;
        gpg = {
          key = "F17DDB98CC3C405C";
          signByDefault = true;
        };
        imap.host = "imap.mailbox.org";
        mbsync = {
          enable = true;
          create = "maildir";
        };
        msmtp.enable = true;
        notmuch.enable = true;
        primary = true;
        realName = config.fullName;
        signature = {
          text = ''
            ---
            Mit freundlichen Grüßen / Best regards
            Demis Balbach
          '';
          showSignature = "append";
        };
        passwordCommand = "pass show Mail/mailbox.org/db@minikn.xyz";
        smtp = {
          host = "smtp.mailbox.org";
        };
        userName = config.primaryMail;
      };
    };
  };
}