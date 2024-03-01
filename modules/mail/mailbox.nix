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
    mail.primary = {
      enable = lib.mkEnableOption "Enable primary email account";

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
  config = lib.mkIf config.mail.primary.enable {

    ## Enabling configurations for email clients if specified
    mail.clients.thunderbird.enable = lib.mkIf (builtins.elem "thunderbird" config.mail.primary.clients) true;
    mail.clients.emacs.enable = lib.mkIf (builtins.elem "emacs" config.mail.primary.clients) true;
    
    home-manager.users.${config.user} = {

      # Enabling thunderbird for this profile if specified
      programs.thunderbird = lib.mkIf (builtins.elem "thunderbird" config.mail.primary.clients) {
        profiles.primary = {
          isDefault = true;
          withExternalGnupg = true;
        };
      };
      
      accounts.email.accounts.primary = {
        thunderbird.enable = lib.mkIf (builtins.elem "thunderbird" config.mail.primary.clients) true;

        maildir.path = "accounts/${config.mail.primary.address}";

        ## General settings for the mail account
        primary = true;
        address = config.mail.primary.address;
        userName = config.mail.primary.address;
        realName = config.fullName;
        imap.host = config.mail.primary.imap-host;
        imap.port = config.mail.primary.imap-port;
        
        passwordCommand = toString (pkgs.writeShellScript "getPassword" ''
           ${pkgs.pass}/bin/pass show Mail/mailbox.org/db@minikn.xyz | head -n 1
        '');

        # imapnotify settings
        imapnotify = {
          enable = true;
          boxes = [ "Inbox" ];
          #onNotify = "${pkgs.isync}/bin/mbsync primary";
          onNotify = "${pkgs.libnotify}/bin/notify-send -t 5000 'You received new private mail.'";
          extraConfig = {
            passwordCmd = toString (pkgs.writeShellScript "getPassword" ''
           ${pkgs.pass}/bin/pass show Mail/mailbox.org/db@minikn.xyz | head -n 1
         '');
          };
        };

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
          create = "both";
          expunge = "both";
          
          groups.primary.channels = {
            inbox = {
              farPattern = "INBOX";
              nearPattern = "inbox";
            };
            sent = {
              farPattern = "Sent";
              nearPattern = "sent";
            };
            drafts = {
              farPattern = "Drafts";
              nearPattern = "drafts";
            };
            trash = {
              farPattern = "Trash";
              nearPattern = "trash";
            };
            spam = {
              farPattern = "Junk";
              nearPattern = "spam";
            };
            archive = {
              farPattern = "Archive";
              nearPattern = "archive";
            };
          };
        };

        ## Signature settings
        signature = {
          text = config.mail.primary.signature;
          showSignature = "append";
        };

        ## smtp settings
        smtp = {
          host = config.mail.primary.smtp-host;
          port = config.mail.primary.smtp-port;
          tls = {
            enable = true;
            useStartTls = true;
          };
        };
      };

      programs.notmuch.hooks = {
        postNew = lib.mkOrder 100 ''
          notmuch tag +personal -- path:accounts/${config.mail.primary.address}/** and tag:new
        '';
      };

      programs.emacs = lib.mkIf (builtins.elem "emacs" config.mail.work.clients) {
        extraConfig = ''
;; ~!emacs-lisp!~
(with-eval-after-load 'db-mail
  (add-to-list 'notmuch-fcc-dirs '("${config.mail.primary.address}" . "accounts/${config.mail.primary.address}/sent"))
  (add-to-list 'notmuch-identities "${config.mail.primary.address}"))
          '';
      };
    };
  };
}
