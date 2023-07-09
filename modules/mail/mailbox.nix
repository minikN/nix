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

  ## Options related to this mail account
  options = {
    mail.${config.primaryMail} = {
      signature = lib.mkOption {
        type = lib.types.lines;
        description = "Default mailbox signature";
        default = ''
            Mit freundlichen Grüßen / Best regards
            Demis Balbach
        '';
      };

      smtp-host = lib.mkOption {
        type = lib.types.str;
        default = "smtp.mailbox.org";
      };
      
      smtp-port = lib.mkOption {
        type = lib.types.int;
        default = 587;
      };
    };
  };

  ## General mail settings
  config = {
     home-manager.users.${config.user}.accounts.email = {
      accounts.${config.primaryMail} = {
        address = config.primaryMail;
        gpg = {
          key = "${config.signingKey}";
          signByDefault = true;
        };
        imap.host = "imap.mailbox.org";
        mbsync = {
          enable = true;
          create = "imap";
        };
        msmtp.enable = true;
        notmuch.enable = true;
        ## this is the primary mail I use
        primary = true;
        realName = config.fullName;
        signature = {
          text = config.mail.${config.primaryMail}.signature;
          showSignature = "append";
        };
        passwordCommand = "pass show Mail/mailbox.org/db@minikn.xyz";
        smtp = {
          host = config.mail.${config.primaryMail}.smtp-host;
          port = config.mail.${config.primaryMail}.smtp-port;
          tls = {
            enable = true;
            useStartTls = true;
          };
        };
        userName = config.primaryMail;
      };
    };
  };
}
