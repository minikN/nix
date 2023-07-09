

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
### Emacs mail configuration
### Source: https://github.com/abcdw/rde/blob/master/src/rde/features/mail.scm
###
### CODE:

{ config, lib, pkgs, ... }:

{
  config = {
    home-manager.users.${config.user} = {
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
                  message-signature "${config.mail.${config.primaryMail}.signature}"))
          
          ;; msmtp
          (setq message-send-mail-function 'message-send-mail-with-sendmail
                message-sendmail-f-is-evil t
                message-sendmail-extra-arguments '("--read-envelope-from"))

          ;; SMTP settings
          (with-eval-after-load 'smtpmail
            (setq smtpmail-smtp-user "${config.primaryMail}"
                  smtpmail-smtp-service "${builtins.toString config.mail.${config.primaryMail}.smtp-port}"
                  smtpmail-smtp-server "${config.mail.${config.primaryMail}.smtp-host}"
                  smtpmail-default-smtp-server "${config.mail.${config.primaryMail}.smtp-host}"
                  
                  smtpmail-stream-type 'starttls
                  smtpmail-queue-dir "${config.home-manager.users.${config.user}.xdg.cacheHome}/emacs/smtpmail/queued-mail"
                  smtpmail-debug-info t))

          ;; sign mails
          (setq mml-secure-openpgp-signers '("${config.signingKey}"))
            ;; (setq mml-secure-openpgp-sign-with-sender t)
            (add-hook 'message-setup-hook 'mml-secure-message-sign-pgpmime)
        '';
      };
    };
  };
}

