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

        ## Emacs configuration
        programs.emacs = {
          extraPackages = epkgs: [ epkgs.notmuch epkgs.ol-notmuch epkgs.cape epkgs.consult epkgs.consult-notmuch ];
          extraConfig = ''
;; ~!emacs-lisp!~
(eval-when-compile
  (require 'message)
  (require 'sendmail))

;; message    
(with-eval-after-load 'message
  (setq message-hidden-headers '()
        message-kill-buffer-on-exit t
        message-signature "${config.mail.primary.signature}"))

;; msmtp
(setq message-send-mail-function 'message-send-mail-with-sendmail
      message-sendmail-f-is-evil t
      message-sendmail-extra-arguments '("--read-envelope-from"))

;; SMTP settings
(with-eval-after-load 'smtpmail
  (setq smtpmail-smtp-user "${config.mail.primary.address}"
        smtpmail-smtp-service "${builtins.toString config.mail.primary.smtp-port}"
        smtpmail-smtp-server "${config.mail.primary.smtp-host}"
        smtpmail-default-smtp-server "${config.mail.primary.smtp-host}"
        
        smtpmail-stream-type 'starttls
        smtpmail-queue-dir "${config.home-manager.users.${config.user}.xdg.cacheHome}/emacs/smtpmail/queued-mail"
        smtpmail-debug-info t))

;; sign mails
(setq mml-secure-openpgp-signers '("${config.const.signingKey}"))
;; (setq mml-secure-openpgp-sign-with-sender t)
(add-hook 'message-setup-hook 'mml-secure-message-sign-pgpmime)

;; notmuch
;; Author: Andrew Tropin <andrew@trop.in>
;;
;; URL: https://trop.in/rde
;; Keywords: convenience

            ;;; Commentary:

;; Set default MUA, adjust view, add auxiliary functions and keybindings.

            ;;; Code:

(eval-when-compile (require 'notmuch))
(autoload 'notmuch-mua-mail "notmuch-mua")
(define-mail-user-agent
  'notmuch-user-agent
  'notmuch-mua-mail
  'notmuch-mua-send-and-exit
  'notmuch-mua-kill-buffer
  'notmuch-mua-send-hook)
(setq mail-user-agent 'notmuch-user-agent)

(with-eval-after-load
    'db-keymaps
  (define-key db-app-map (kbd "n") 'notmuch))

(define-key global-map (kbd "M-s n") 'consult-notmuch-tree)
(define-key global-map (kbd "s-m") 'notmuch-jump-search)
(setq notmuch-saved-searches
      '((:name "To Process" :query "tag:todo" :key "t")
        (:name "Drafts" :query "tag:draft" :key "d")
        (:name "Watching" :query "thread:{tag:watch} and tag:unread" :key "w")
        (:name "Work Inbox" :query "tag:work and tag:inbox" :key "W")
        (:name "Personal Inbox" :query "tag:personal and tag:inbox" :key "P")))
(setq notmuch-search-oldest-first nil)
(with-eval-after-load 'notmuch-mua (require 'notmuch))
          '';
        };
    };
  };
}
