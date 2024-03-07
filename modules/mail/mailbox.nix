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
  ## Options related to this mail account
  options = {
    mail.private = {
      enable = lib.mkEnableOption "Enable private email account";

      address = lib.mkOption {
        type = lib.types.str;
        default = "db@minikn.xyz";
      };

      signature = lib.mkOption {
        type = lib.types.lines;
        description = "Default mailbox signature";
        default = ''
            Mit freundlichen Grüßen / Best regards
            Demis Balbach
        '';
      };

      clients = lib.mkOption {
        type = lib.types.listOf lib.types.str;
        description = "List of clients to use for this account";
        default = [];
      };

      imap-host = lib.mkOption {
        type = lib.types.str;
        default = "imap.mailbox.org";
      };
      
      imap-port = lib.mkOption {
        type = lib.types.int;
        default = 993;
      };

      smtp-host = lib.mkOption {
        type = lib.types.str;
        default = "smtp.mailbox.org";
      };
      
      smtp-port = lib.mkOption {
        type = lib.types.int;
        default = 465;
      };
    };
  };

  ## General mail settings
  config = lib.mkIf config.mail.private.enable {

    ## Setting this account as primary system wide
    mail.address = lib.mkForce config.mail.private.address;
    mail.signature = lib.mkForce config.mail.private.signature;

    ## Enabling configurations for email clients if specified
    mail.clients.thunderbird.enable = lib.mkIf (builtins.elem "thunderbird" config.mail.private.clients) true;
    mail.clients.thunderbird.primary = lib.mkForce (lib.mkIf (builtins.elem "thunderbird" config.mail.private.clients) config.mail.private.address);
    
    mail.clients.emacs.enable = lib.mkIf (builtins.elem "emacs" config.mail.private.clients) true;
    mail.clients.emacs.primary = lib.mkForce (lib.mkIf (builtins.elem "emacs" config.mail.private.clients) config.mail.private.address);

    home-manager.users.${config.user} = {

      # Enabling thunderbird for this profile if specified
      programs.thunderbird = lib.mkIf (builtins.elem "thunderbird" config.mail.private.clients) {
        profiles.private = {
          isDefault = true;
          withExternalGnupg = true;
        };
      };

      accounts.email.accounts.private = {
        thunderbird.enable = lib.mkIf (builtins.elem "thunderbird" config.mail.private.clients) true;

        maildir.path = "accounts/${config.mail.private.address}";

        ## General settings for the mail account
        primary = true;
        address = config.mail.private.address;
        userName = config.mail.private.address;
        realName = config.fullName;
        imap.host = config.mail.private.imap-host;
        imap.port = config.mail.private.imap-port;


        ## We need to expose these vars so the mbsync service knows of them
        passwordCommand = toString (pkgs.writeShellScript "get-private-password" ''
## ~!shell!~
export GNUPGHOME=${config.home-manager.users.${config.user}.programs.gpg.homedir}
export PASSWORD_STORE_DIR=${config.const.passDir}
${pkgs.pass}/bin/pass show Mail/mailbox.org/db@minikn.xyz | ${pkgs.coreutils}/bin/head -n 1
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
          groups.private.channels = {
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
              farPattern = "Junk";
              nearPattern = "spam";
            };
            archive = {
              extraConfig = { Create = "both"; Expunge = "both"; };
              farPattern = "Archive";
              nearPattern = "archive";
            };
          };
        };

        ## Signature settings
        signature = {
          text = config.mail.private.signature;
          showSignature = "append";
        };

        ## smtp settings
        smtp = {
          host = config.mail.private.smtp-host;
          port = config.mail.private.smtp-port;
          tls = {
            enable = true;
            useStartTls = true;
          };
        };
      };

      programs.emacs = lib.mkIf (builtins.elem "emacs" config.mail.private.clients) {
        extraConfig = ''
;; ~!emacs-lisp!~
(with-eval-after-load 'db-mail
  (add-to-list 'notmuch-fcc-dirs '("${config.mail.private.address}" . "accounts/${config.mail.private.address}/sent"))
  (add-to-list 'notmuch-identities "${config.mail.private.address}"))
          '';
      };
    };
  };
}
