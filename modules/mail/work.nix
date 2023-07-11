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
### Work mail configuration
###
### CODE:

{ config, lib, pkgs, ... }:

{
  ## Options related to this mail account
  options = {
    mail.work = {
      enable = lib.mkEnableOption "Enable work email account";

      address = lib.mkOption {
        type = lib.types.str;
        default = "demis.balbach@apprologic.de";
      };

      signature = lib.mkOption {
        type = lib.types.lines;
        description = "Default work signature";
        default = ''
            Mit freundlichen Grüßen / Best regards
            Demis Balbach
            ApproLogic GmbH
        '';
      };

      imap-host = lib.mkOption {
        type = lib.types.str;
        default = "wp168.webpack.hosteurope.de";
      };
      
      imap-port = lib.mkOption {
        type = lib.types.int;
        default = 993;
      };

      smtp-host = lib.mkOption {
        type = lib.types.str;
        default = "wp168.webpack.hosteurope.de";
      };
      
      smtp-port = lib.mkOption {
        type = lib.types.int;
        default = 587;
      };
    };
  };

  ## General mail settings
  config = lib.mkIf config.mail.work.enable {
     home-manager.users.${config.user}.accounts.email = {
      accounts.work = {
        ## General settings for the mail account
        primary = false;
        address = config.mail.work.address;
        userName = "wp10718698-balbach";
        realName = config.fullName;
        imap.host = config.mail.work.imap-host;

        ## We need to expose these vars so the mbsync service knows of them
        #passwordCommand = "GNUPGHOME=${config.home-manager.users.${config.user}.programs.gpg.homedir} PASSWORD_STORE_DIR=${config.const.passDir} ${pkgs.pass}/bin/pass show Mail/apprologic.de/demis.balbach@apprologic.de";
        #passwordCommand = "${pkgs.pass}/bin/pass show Mail/apprologic.de/demis.balbach@apprologic.de";
        passwordCommand = "${pkgs.coreutils}/bin/env GNUPGHOME=${config.home-manager.users.${config.user}.programs.gpg.homedir} PASSWORD_STORE_DIR=${config.const.passDir} ${pkgs.pass}/bin/pass show Mail/apprologic.de/demis.balbach@apprologic.de";

        ## IMAP folder mapping
        folders = {
          drafts = "Entwurf";
          sent = "Sent";
          trash = "Trash";
        };

        # imapnotify settings
        imapnotify = {
          enable = true;
          boxes = [ "Inbox" ];
        };

        ## Enable features
        msmtp.enable = true;
        notmuch.enable = true;

        ## Sign emails by default
        gpg = {
          key = "${config.const.signingKey}";
          signByDefault = true;
        };

        ## mbsync settings
        mbsync = {
          enable = true;
          create = "imap";
        };

        ## Signature settings
        signature = {
          text = config.mail.work.signature;
          showSignature = "append";
        };

        ## smtp settings
        smtp = {
          host = config.mail.work.smtp-host;
          port = config.mail.work.smtp-port;
          tls = {
            enable = true;
            useStartTls = true;
          };
        };
      };
    };
  };
}
