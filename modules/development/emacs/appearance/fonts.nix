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
### Emacs fonts configuration
### Source: https://github.com/abcdw/rde/blob/master/src/rde/features/emacs-xyz.scm
###
### CODE:

{ config, lib, pkgs, ... }:

{
  config = {
    home-manager.users.${config.user} = {
      programs.emacs = {
        extraPackages = epkgs: [ epkgs.fontaine ];
        extraConfig = ''
        (eval-when-compile (require 'cl-macs) (require 'subr-x))
        (defvar db-fonts-emoji-list nil "Cached list of emojis.")
        (defun db-fonts--build-emojis ()
          "Create an emoji list by looping over the total range of characters."
          (delete
            nil
            (cl-loop with range = '(126976 . 129535)
              for i upto (- (cdr range) (car range))
              collect (when-let* ((codepoint (+ (car range) i))
                                  (name (get-char-code-property codepoint 'name)))
                                  (thread-last
                                  (replace-regexp-in-string " " "-" (downcase name))
                                  (format ":%s:")
                                  (format "%s %s" (char-to-string (char-from-name name))))))))
        (defun db-fonts-insert-emoji ()
          "Insert an emoji character to the current buffer."
          (interactive)
          (thread-first
            (completing-read
              "Select emoji: "
              (or db-fonts-emoji-list
                  (setq db-fonts-emoji-list (db-fonts--build-emojis))))
            (substring 0 1)
            (insert)))
        
        (define-key search-map "e" 'db-fonts-insert-emoji)
        (define-key minibuffer-mode-map (kbd "C-c C-e") 'db-fonts-insert-emoji)
        
        (with-eval-after-load
          'fontset
          (set-fontset-font t 'symbol "Unifont" nil 'append)
          (set-fontset-font t 'unicode "Unifont" nil 'append)
          (set-fontset-font "fontset-default" nil (font-spec :name "Unifont")))
        
        (setq use-default-font-for-symbols nil)
        
        (require 'fontaine)
        (setq fontaine-presets
              '((t :default-family "${config.os.fonts.mono.regular}"
                  :default-height ${toString (config.os.fonts.size * 10)}
                  :fixed-pitch-family "${config.os.fonts.mono.regular}"
                  :fixed-pitch-height 1.0
                  :variable-pitch-family "${config.os.fonts.sans.regular}"
                  :variable-pitch-height 1.0
                  :variable-pitch-weight regular)))
        
        (require 'xdg)
        (setq fontaine-latest-state-file
              (expand-file-name "emacs/fontaine-latest.state.eld"
                                (or (xdg-cache-home) "~/.cache")))

        (defun db-font--set-default-fonts ()
          (fontaine-set-preset t))
        
        (if after-init-time
          (when (display-graphic-p) (db-font--set-default-fonts))
          (add-hook 'after-init-hook 'db-font--set-default-fonts))
        
        (add-hook
          'db-modus-themes-after-enable-theme-hook
          'fontaine-apply-current-preset)
        '';
      };
    };
  };
}

