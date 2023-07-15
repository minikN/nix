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

        systemd.user.services.imap-primary.Services.ExecStart =
        lib.mkForce "${lib.getExe config.home-manager.users.${config.user}.services.imapnotify.package} -debug -conf '${config.home-manager.users.${config.user}.xdg.configHome}/imapnotify/imapnotify-primary-config.json'";
        
        accounts.email.maildirBasePath = config.const.mailDir;

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
          hooks = {
            preNew = ''
              move-tagged-messages () {
                for f in $(${pkgs.notmuch}/bin/notmuch search --output=files "not path:/.*\\/$1/ and tag:$1" | ${pkgs.gnugrep}/bin/grep -v \"/$2/\"); do
                  ${pkgs.coreutils}/bin/mv -v $f $(${pkgs.coreutils}/bin/echo $f | ${pkgs.coreutils}/bin/sed "s;/[[:alnum:]]*/cur/;/$1/cur/;" | ${pkgs.coreutils}/bin/sed 's/,U=[0-9]*:/:/');
                done
              }

              move-out-untagged-messages () {
                for f in $(${pkgs.notmuch}/bin/notmuch search --output=files "path:/.*\\/$1/ and not tag:$1" | ${pkgs.gnugrep}/bin/grep "/$1/"); do
                  mv -v $f $(${pkgs.coreutils}/bin/echo $f | ${pkgs.coreutils}/bin/sed "s;/$1/;/archive/;" | ${pkgs.coreutils}/bin/sed 's/,U=[0-9]*:/:/');
                done
              }

              ## 1
              move-tagged-messages "inbox" "archive"

              ## 2
              for f in "inbox" "trash" "spam"; do 
                move-tagged-messages $f "nothing-will-match-this"
              done

              ## 3
              for f in "inbox" "trash" "spam"; do 
                move-out-untagged-messages $f
              done
            '';

            postNew = ''
              ${pkgs.notmuch}/bin/notmuch tag -new -- tag:new
              ${pkgs.notmuch}/bin/notmuch tag +inbox -- path:/.*\\/inbox/
              ${pkgs.notmuch}/bin/notmuch tag +draft -- path:/.*\\/drafts/
              ${pkgs.notmuch}/bin/notmuch tag +sent  -- path:/.*\\/sent/
              ${pkgs.notmuch}/bin/notmuch tag +trash -- path:/.*\\/trash/
              ${pkgs.notmuch}/bin/notmuch tag +spam  -- path:/.*\\/spam/
              ${pkgs.notmuch}/bin/notmuch tag +todo -inbox -sent  -- tag:inbox and tag:sent
              ## If file was moved out of folder on server remove respective tag
              ${pkgs.notmuch}/bin/notmuch tag -inbox -- not path:/.*\\/inbox/ and tag:inbox
              ${pkgs.notmuch}/bin/notmuch tag -trash -- not path:/.*\\/trash/ and tag:trash
              ${pkgs.notmuch}/bin/notmuch tag -spam  -- not path:/.*\\/spam/  and tag:spam
          '';
          };
        };

        ## Emacs configuration
        programs.emacs = {
          extraPackages = epkgs: [ epkgs.notmuch epkgs.ol-notmuch ];
          extraConfig = ''
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
            (eval-when-compile (require 'notmuch))
            (autoload 'notmuch-mua-mail "notmuch-mua")
            ;; Copied definition from notmuch-mua becasue it's not
            ;; available until notmuch-mua loaded.

            ;; FIXME: C-x m doesn't accept prefix argument, so I can't
            ;; pick sender.
            (define-mail-user-agent 'notmuch-user-agent
              'notmuch-mua-mail
              'notmuch-mua-send-and-exit
              'notmuch-mua-kill-buffer
              'notmuch-mua-send-hook)
            (setq mail-user-agent 'notmuch-user-agent)

            (define-key user-app-map (kbd "n") 'notmuch)

            ;; TODO: consult
            ;; (define-key global-map (kbd "M-s n") 'consult-notmuch-tree)
            (define-key global-map (kbd "s-m") 'notmuch-jump-search)

            (setq notmuch-saved-searches
              '((:name "TODO" :query "tag:todo" :key "t")
                (:name "Inbox" :query "tag:inbox" :key "i")
                (:name "Watching" :query "thread:{tag:watch} and tag:unread" :key "w")
                (:name "Drafts" :query "tag:draft" :key "d")
                (:name "Flagged" :query "tag:flagged" :key "f")
                (:name "Sent" :query "tag:sent" :key "s")
                (:name "All mail" :query "*" :key "a")))
          '';
        };
    };
  };
}