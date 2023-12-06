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

        services = {
          imapnotify = {
            enable = true;
          };
        };
        programs.mbsync.enable = true;
        programs.msmtp.enable = true;
      
        services.mbsync = {
          enable = true;
          preExec = toString (pkgs.writeShellScript "mbsync-pre" ''
## ~!shell!~
function safeMove { s=$\\{1##*/}; s=$\\{s%%,*}; mv -f $1 $2/$s; }
mkdir -p ${config.const.mailDir}
for f in $(ls ${config.const.mailDir}/accounts); do
    mkdir -p ${config.const.mailDir}/$f
    for i in $(${pkgs.notmuch}/bin/notmuch search --exclude=false --output=files path:/.\*\/$f/ AND tag:trash AND NOT folder:trash); do
	safeMove $i ${config.const.mailDir}/accounts/$f/trash/cur
    done
    for i in $(${pkgs.notmuch}/bin/notmuch search --exclude=false --output=files path:/.\*\/$f/ AND tag:deleted AND NOT folder:trash); do
	safeMove $i ${config.const.mailDir}/accounts/$f/trash/cur
    done
    for i in $(${pkgs.notmuch}/bin/notmuch search --exclude=false --output=files path:/.\*\/$f/ AND tag:spam AND NOT folder:spam); do
	safeMove $i ${config.const.mailDir}/accounts/$f/spam/cur
    done
    for i in $(${pkgs.notmuch}/bin/notmuch search --exclude=false --output=files path:/.\*\/$f/ AND tag:archive AND NOT folder:archive); do
	safeMove $i ${config.const.mailDir}/accounts/$f/archive/cur
    done
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
          hooks = {
            #preNew = ''
            #  for f in $(ls ${config.const.mailDir}); do
            #    notmuch search --exclude=false --output=files path:/.\*\/$f/ AND tag:spam AND NOT folder:spam
            #  done            
            #'';

            postNew = lib.mkOrder 200 ''
              notmuch tag +inbox -- path:/accounts\\/.*\\/inbox/
              notmuch tag +draft -- path:/accounts\\/.*\\/drafts/
              notmuch tag +sent  -- path:/accounts\\/.*\\/sent/
              notmuch tag +trash -- path:/accounts\\/.*\\/trash/
              notmuch tag +spam  -- path:/accounts\\/.*\\/spam/
              notmuch tag -inbox -- not path:/accounts\\/.*\\/inbox/ and tag:inbox
              notmuch tag -trash -- not path:/accounts\\/.*\\/trash/ and tag:trash
              notmuch tag -spam  -- not path:/accounts\\/.*\\/spam/  and tag:spam
              notmuch tag -new -- tag:new
            '';
          };
        };

        ## Emacs configuration
        programs.emacs = {
          extraPackages = epkgs: [ epkgs.notmuch epkgs.ol-notmuch epkgs.cape epkgs.consult epkgs.consult-notmuch ];
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
            (with-eval-after-load
              'notmuch
              (setq notmuch-address-use-company nil)
              (defun rde-notmuch-address-setup () "Function doing nothing" nil)
              (advice-add 'notmuch-address-setup :override 'rde-notmuch-address-setup)
              (require 'cape)
              (defun rde-notmuch-message-mode
                    ()
                    "Add completion at point functions made from company backends."
                    (setq-local
                      completion-at-point-functions
                      (append
                        (list (cape-company-to-capf 'notmuch-company))
                        completion-at-point-functions)))
              (add-hook 'notmuch-message-mode-hook 'rde-notmuch-message-mode)
              (setq notmuch-unthreaded-show-out nil)
              (setq notmuch-show-empty-saved-searches t)
              (setq notmuch-mua-cite-function 'message-cite-original-without-signature)
              (defvar rde-notmuch-todo-tags '("+todo" "-inbox"))
              (defvar rde-notmuch-spam-tags '("+spam" "-inbox"))
              (defvar rde-notmuch-trash-tags '("+trash" "-inbox" "-draft"))
              (defvar rde-notmuch-delete-tags '("+deleted" "-inbox" "-draft"))
              (setq notmuch-archive-tags '("-inbox" "-todo"))
              (setq notmuch-tagging-keys
                    '(("a" notmuch-archive-tags "Archive")
                      ("r" notmuch-show-mark-read-tags "Mark read")
                      ("f" ("+flagged") "Flag (favorite)")
                      ("t" rde-notmuch-todo-tags "Mark as todo")
                      ("s" rde-notmuch-spam-tags "Mark as spam")
                      ("d" rde-notmuch-trash-tags "Trash")
                      ("D" rde-notmuch-delete-tags "Delete")))
              (defun rde-notmuch-show-view-as-patch
                    ()
                    "View the the current message as a patch."
                    (interactive)
                    (let* ((id (notmuch-show-get-message-id))
                            (msg (notmuch-show-get-message-properties))
                            (part (notmuch-show-get-part-properties))
                            (subject (concat "Subject: " (notmuch-show-get-subject) "\n"))
                            (diff-default-read-only t)
                            (buf (get-buffer-create (concat "*notmuch-patch-" id "*")))
                            (map (make-sparse-keymap)))
                      (define-key map "q" 'notmuch-bury-or-kill-this-buffer)
                      (switch-to-buffer buf)
                      (let ((inhibit-read-only t))
                        (erase-buffer)
                        (insert subject)
                        (insert (notmuch-get-bodypart-text msg part nil)))
                      (set-buffer-modified-p nil)
                      (diff-mode)
                      (let ((new-ro-bind (cons 'buffer-read-only map)))
                        (add-to-list 'minor-mode-overriding-map-alist new-ro-bind))
                      (goto-char (point-min))))
              (define-key 'notmuch-show-part-map "d" 'rde-notmuch-show-view-as-patch)
              (define-key
                notmuch-search-mode-map
                "w"
                (lambda ()
                  (interactive)
                  (notmuch-tag
                    (concat
                      "id:"
                      (car (notmuch-query-get-message-ids
                            (notmuch-search-find-thread-id))))
                    (list "+watch"))
                  (notmuch-tree-next-message)))
              (define-key
                notmuch-search-mode-map
                "d"
                (lambda ()
                  (interactive)
                  (notmuch-search-add-tag rde-notmuch-trash-tags)
                  (notmuch-tree-next-message)))
              (define-key
                notmuch-search-mode-map
                "D"
                (lambda ()
                  (interactive)
                  (notmuch-search-add-tag rde-notmuch-delete-tags)
                  (notmuch-tree-next-message)))
              (define-key
                notmuch-search-mode-map
                "T"
                (lambda ()
                  (interactive)
                  (notmuch-search-add-tag rde-notmuch-todo-tags)
                  (notmuch-tree-next-message)))
              (defun rde-notmuch-show-view-html-part
                    ()
                    "Open the text/html part of the current message using\n`notmuch-show-view-part'."
                    (interactive)
                    (save-excursion
                      (goto-char
                        (prop-match-beginning
                          (text-property-search-forward
                            :notmuch-part
                            "text/html"
                            (lambda (value notmuch-part)
                              (equal (plist-get notmuch-part :content-type) value)))))
                      (notmuch-show-view-part)))
              (define-key notmuch-show-part-map "h" 'rde-notmuch-show-view-html-part)
              (defun rde-notmuch-tree-insert-tree
                    (tree depth tree-status first last)
                    "Insert the message tree TREE at depth DEPTH in the current thread.\n\nA message tree is another name for a single sub-thread: i.e., a\nmessage together with all its descendents."
                    (let ((msg (car tree)) (replies (cadr tree)))
                      (cond ((and (< 0 depth) (not last)) (push "├" tree-status))
                            ((and (< 0 depth) last) (push "└" tree-status))
                            ((and (eq 0 depth) first last) (push " " tree-status))
                            ((and (eq 0 depth) first (not last)) (push "┬" tree-status))
                            ((and (eq 0 depth) (not first) last) (push "└" tree-status))
                            ((and (eq 0 depth) (not first) (not last))
                              (push "├" tree-status)))
                      (unless
                        (eq 0 depth)
                        (push (concat (if replies "┬" "─") ">") tree-status))
                      (setq msg (plist-put msg :first (and first (eq 0 depth))))
                      (setq msg (plist-put msg :tree-status tree-status))
                      (setq msg (plist-put msg :orig-tags (plist-get msg :tags)))
                      (notmuch-tree-goto-and-insert-msg msg)
                      (pop tree-status)
                      (pop tree-status)
                      (if last (push " " tree-status) (push "│" tree-status))
                      (notmuch-tree-insert-thread replies (+ 1 depth) tree-status)))
              (advice-add
                'notmuch-tree-insert-tree
                :override
                'rde-notmuch-tree-insert-tree)
              (defun rde-notmuch-jump
                    (action-map prompt)
                    "Interactively prompt for one of the keys in ACTION-MAP.\n\nDisplays a summary of all bindings in ACTION-MAP in the\nminibuffer, reads a key from the minibuffer, and performs the\ncorresponding action.  The prompt can be canceled with C-g or\nRET.  PROMPT must be a string to use for the prompt.  PROMPT\nshould include a space at the end.\n\nACTION-MAP must be a list of triples of the form\n  (KEY LABEL ACTION)\nwhere KEY is a key binding, LABEL is a string label to display in\nthe buffer, and ACTION is a nullary function to call.  LABEL may\nbe null, in which case the action will still be bound, but will\nnot appear in the pop-up buffer."
                    (let* ((items (notmuch-jump--format-actions action-map))
                            (table (with-temp-buffer
                                    (notmuch-jump--insert-items
                                      (floor (* (frame-width) 0.8))
                                      items)
                                    (buffer-string)))
                            (full-prompt
                              (concat
                                table
                                "\n\n"
                                (propertize prompt 'face 'minibuffer-prompt)))
                            (minibuffer-prompt-properties
                              (notmuch-plist-delete
                                (copy-sequence minibuffer-prompt-properties)
                                'face))
                            (minibuffer-map (notmuch-jump--make-keymap action-map prompt))
                            (notmuch-jump--action nil))
                      (read-from-minibuffer full-prompt nil minibuffer-map)
                      (when notmuch-jump--action (funcall notmuch-jump--action))))
              (advice-add 'notmuch-jump :override 'rde-notmuch-jump)
              (setq notmuch-search-result-format
                    '(("date" . "%12s ")
                      ("count" . "%-7s ")
                      ("authors" . "%-20s ")
                      ("subject" . "%-80s  ")
                      ("tags" . "(%s)")))
              (setq notmuch-tree-result-format
                    '(("date" . "%12s  ")
                      ("authors" . "%-20s")
                      ((("tree" . "%s") ("subject" . "%s")) . " %-88s ")
                      ("tags" . "(%s)")))
              (setq notmuch-unthreaded-result-format
                    '(("date" . "%12s  ")
                      ("authors" . "%-20s")
                      ((("subject" . "%s")) . " %-88s ")
                      ("tags" . "(%s)")))
              (setq notmuch-show-logo nil))
          '';
        };
    };
  };
}
