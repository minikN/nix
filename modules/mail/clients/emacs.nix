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
### Emacs window management configuration
###
### CODE:

{ config, lib, pkgs, inputs, ... }:

{
  config = let
    utils = import ./../../../utils.nix { inherit lib pkgs config; };
  in lib.mkIf config.mail.clients.emacs.enable {
    home-manager.users.${config.user} = {
      programs.emacs = utils.emacsPkg {
        name = "db-mail";
        description = "Mail configuration";
        require = true;
        packages = [
          pkgs.emacsPackages.notmuch
          pkgs.emacsPackages.ol-notmuch
          pkgs.emacsPackages.consult-notmuch # FIX: Check if consult module is enabled
        ];
        code = ''
;; ~!emacs-lisp!~
;; (eval-when-compile
;;   (require 'message)
;;   (require 'sendmail))

;; ;; msmtp
;; (setq message-send-mail-function 'message-send-mail-with-sendmail
;;       message-sendmail-f-is-evil t
;;       message-sendmail-extra-arguments '("--read-envelope-from"))

;; ;; sign mails
;; (setq mml-secure-openpgp-signers '("${config.const.signingKey}"))
;; ;; (setq mml-secure-openpgp-sign-with-sender t)
;; (add-hook 'message-setup-hook 'mml-secure-message-sign-pgpmime)

;; ;; notmuch
;; ;; Author: Andrew Tropin <andrew@trop.in>
;; ;;
;; ;; URL: https://trop.in/rde
;; ;; Keywords: convenience

;;             ;;; Commentary:

;; ;; Set default MUA, adjust view, add auxiliary functions and keybindings.

;;             ;;; Code:

;; 					;(eval-when-compile (require 'notmuch))
;; (autoload 'notmuch-mua-mail "notmuch-mua")
;; (define-mail-user-agent
;;   'notmuch-user-agent
;;   'notmuch-mua-mail
;;   'notmuch-mua-send-and-exit
;;   'notmuch-mua-kill-buffer
;;   'notmuch-mua-send-hook)
;; (setq mail-user-agent 'notmuch-user-agent)

;; (with-eval-after-load
;;     'db-keymaps
;;   (define-key db-app-map (kbd "n") 'notmuch))

;; (define-key global-map (kbd "M-s n") 'consult-notmuch-tree)
;; (define-key global-map (kbd "s-m") 'notmuch-jump-search)

(if (not (boundp 'notmuch-fcc-dirs))
    (setq notmuch-fcc-dirs '()))
(if (not (boundp 'notmuch-identities))
    (setq notmuch-identities '()))

;; (setq notmuch-saved-searches
;;       '((:name "To Process" :query "tag:todo" :key "t")
;;         (:name "Drafts" :query "tag:draft" :key "d")
;;         (:name "Watching" :query "thread:{tag:watch} and tag:unread" :key "w")
;;         (:name "Work Inbox" :query "tag:work and tag:inbox" :key "W")
;;         (:name "Personal Inbox" :query "tag:personal and tag:inbox" :key "P")))
;; (setq notmuch-search-oldest-first nil)
;; (with-eval-after-load 'notmuch-mua (require 'notmuch))
        '';
        config = ''
;; ~!emacs-lisp!~
(use-package notmuch)
'';
      };
    };
  };
}
