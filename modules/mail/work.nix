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

      clients = lib.mkOption {
        type = lib.types.listOf lib.types.str;
        description = "List of clients to use for this account";
        default = [];
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

    ## Setting this account as primary system wide
    mail.address = lib.mkDefault config.mail.work.address;
    mail.signature = lib.mkDefault config.mail.work.signature;

    ## Enabling configurations for email clients if specified
    mail.clients.thunderbird.enable = lib.mkIf (builtins.elem "thunderbird" config.mail.work.clients) true;
    mail.clients.thunderbird.primary = lib.mkDefault (lib.mkIf (builtins.elem "thunderbird" config.mail.work.clients) config.mail.private.address);
    
    mail.clients.emacs.enable = lib.mkIf (builtins.elem "emacs" config.mail.work.clients) true;
    mail.clients.emacs.primary = lib.mkDefault (lib.mkIf (builtins.elem "emacs" config.mail.work.clients) config.mail.private.address);

    home-manager.users.${config.user} = {

      # Enabling thunderbird for this profile if specified
      programs.thunderbird = lib.mkIf (builtins.elem "thunderbird" config.mail.work.clients) {
        profiles.work = {
          ## If thunderbird is set as a mail client in the primary account, this one should be set
          ## as default
          isDefault = lib.mkIf (builtins.elem "thunderbird" config.mail.private.clients == false) true;
          withExternalGnupg = true;
        };
      };

      accounts.email.accounts.work = {
        thunderbird.enable = lib.mkIf (builtins.elem "thunderbird" config.mail.work.clients) true;

        maildir.path = "accounts/${config.mail.work.address}";

        ## General settings for the mail account
        primary = lib.mkIf (config.mail.address == config.mail.work.address) true;
        address = config.mail.work.address;
        userName = "wp10718698-balbach";
        realName = config.fullName;
        imap.host = config.mail.work.imap-host;
        imap.port = config.mail.work.imap-port;

        ## We need to expose these vars so the mbsync service knows of them
        passwordCommand = toString (pkgs.writeShellScript "get-work-password" ''
## ~!shell!~
export GNUPGHOME=${config.home-manager.users.${config.user}.programs.gpg.homedir}
export PASSWORD_STORE_DIR=${config.const.passDir}
${pkgs.pass}/bin/pass show Mail/apprologic.de/demis.balbach@apprologic.de | ${pkgs.coreutils}/bin/head -n 1
'');

        ## Enable features
        msmtp.enable = true;
        notmuch.enable = true;

        ## Sign emails by default
        gpg = {
          key = "${config.const.signingKey}";
          signByDefault = true;
        };

        ## IMAP folder mapping
        folders.inbox = "inbox";

        ## mbsync settings
        mbsync = {
          enable = true;
          groups.work.channels = {
            inbox = {
              extraConfig = { Create = "both"; Expunge = "both"; };
              farPattern = "INBOX";
              nearPattern = "inbox";
            };
            sent = {
              extraConfig = { Create = "both"; Expunge = "both"; };
              farPattern = "Sent";
              nearPattern = "sent";
            };
            drafts = {
              extraConfig = { Create = "both"; Expunge = "both"; };
              farPattern = "Drafts";
              nearPattern = "drafts";
            };
            trash = {
              extraConfig = { Create = "both"; Expunge = "both"; };
              farPattern = "Trash";
              nearPattern = "trash";
            };
            spam = {
              extraConfig = { Create = "both"; Expunge = "both"; };
              farPattern = "Spam";
              nearPattern = "spam";
            };
            archive = {
              extraConfig = { Create = "both"; Expunge = "both"; };
              farPattern = "All";
              nearPattern = "archive";
            };
          };
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

      programs.emacs = lib.mkIf (builtins.elem "emacs" config.mail.work.clients) {
        extraConfig = ''
;; ~!emacs-lisp!~
(with-eval-after-load 'db-mail
  (add-to-list 'notmuch-fcc-dirs '("${config.mail.work.address}" . "accounts/${config.mail.work.address}/sent"))
  (add-to-list 'notmuch-identities "${config.mail.work.address}"))
          '';
      };
    };
  };
}
