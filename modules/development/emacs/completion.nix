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
### Emacs completion configuration
### Source: https://github.com/abcdw/rde/blob/master/src/rde/features/emacs-xyz.scm#L154
###
### CODE:

{ config, lib, pkgs, inputs, ... }:

{ 
  config = {

    home-manager.users.${config.user} = {
      programs.emacs = {
        extraPackages = epkgs: [
          epkgs.consult
          epkgs.embark
          epkgs.embark-consult
          epkgs.corfu
          epkgs.cape
          epkgs.marginalia
          epkgs.pcmpl-args
          epkgs.orderless
        ];
        extraConfig = ''
          (eval-when-compile (require 'marginalia) (require 'consult))
          (defgroup ${config.user}-completion nil
            "Tweaks to the built-in Emacs completion."
            :group 'rde)

          (with-eval-after-load
            'minibuffer

            ;; It's a little easier to press C-i than C-M-i
            (setq tab-always-indent 'complete)
            (setq minibuffer-prompt-properties
                  '(read-only t cursor-intangible t face minibuffer-prompt))
            (add-hook 'minibuffer-setup-hook 'cursor-intangible-mode)

            (setq completion-show-help nil)
            (setq completions-format 'one-column)
            (setq completions-header-format nil)

            (let ((map minibuffer-mode-map))
              (define-key map (vector 'remap 'next-line) 'minibuffer-next-completion)
              (define-key map (vector 'remap 'previous-line) 'minibuffer-previous-completion))
            
            (let ((map completion-in-region-mode-map))
              (define-key map (kbd "C-n") 'minibuffer-next-completion)
              (define-key map (kbd "C-p") 'minibuffer-previous-completion))
            
            (add-hook 'rfn-eshadow-update-overlay-hook 'vertico-directory-tidy)
            
            ;; Needed for orderless in default completion UI.
            (let ((map minibuffer-local-completion-map))
              (define-key map (kbd "SPC") nil)
              (define-key map (kbd "?") nil))
            
            ;; Allows to use \\SPC instead of \\s-
            (setq orderless-component-separator 'orderless-escapable-split-on-space)
            
            (defun ${config.user}-orderless-literal-dispatcher (pattern _index _total)
              "Literal style dispatcher using the equals sign as a suffix.
              It matches PATTERN _INDEX and _TOTAL according to how Orderless
              parses its input."
              (cond ((equal "=" pattern) '(orderless-literal . "="))
                    ((string-suffix-p "=" pattern)
                      (cons 'orderless-literal (substring pattern 0 -1)))))
            
            (defun ${config.user}-orderless-without-literal-dispatcher (pattern _index _total)
              "Literal without style dispatcher using the exclamation mark as a
              suffix.  It matches PATTERN _INDEX and _TOTAL according to how Orderless
              parses its input."
              (cond ((equal "!" pattern) '(orderless-literal . "!"))
                    ((string-suffix-p "!" pattern)
                      (cons 'orderless-without-literal (substring pattern 0 -1)))))
            
            (defun ${config.user}-orderless-initialism-dispatcher (pattern _index _total)
              "Leading initialism  dispatcher using the comma suffix.
              It matches PATTERN _INDEX and _TOTAL according to how Orderless
              parses its input."
              (cond ((equal "," pattern) '(orderless-literal . ","))
                    ((string-suffix-p "," pattern)
                      (cons 'orderless-initialism (substring pattern 0 -1)))))
            
            (defun ${config.user}-orderless-flex-dispatcher (pattern _index _total)
              "Flex  dispatcher using the tilde suffix.
              It matches PATTERN _INDEX and _TOTAL according to how Orderless
              parses its input."
              (cond ((equal "~" pattern) '(orderless-literal . "~"))
                    ((string-suffix-p "~" pattern)
                      (cons 'orderless-flex (substring pattern 0 -1)))))

            (defcustom ${config.user}-completion-initial-narrow-alist '()
              "Alist of MODE . KEY to present an initial completion narrowing via\n `consult'."
              :group '${config.user}-completion
              :type 'list)
            
            (defun ${config.user}-completion--mode-buffers (&rest modes)
              "Return a list of buffers that are derived from MODES in `buffer-list'."
              (cl-remove-if-not
                (lambda (buffer)
                  (with-current-buffer buffer (cl-some 'derived-mode-p modes)))
                (buffer-list)))

            (defun ${config.user}-completion-initial-narrow ()
              "Set initial narrow source for buffers under a specific mode."
              (let* ((buffer-mode-assoc ${config.user}-completion-initial-narrow-alist)
                      (key (and (eq this-command 'consult-buffer)
                                (or (alist-get
                                      (buffer-local-value
                                        'major-mode
                                        (window-buffer (minibuffer-selected-window)))
                                      buffer-mode-assoc)
                                    (cdr (cl-find-if
                                          (lambda (mode)
                                            (with-current-buffer
                                              (window-buffer
                                                (minibuffer-selected-window))
                                              (derived-mode-p (car mode))))
                                          buffer-mode-assoc))))))
                    (when key
                          (setq unread-command-events
                                (append unread-command-events (list key 32))))))

            (add-hook 'minibuffer-setup-hook '${config.user}-completion-initial-narrow)
            (setq orderless-style-dispatchers
                  '(${config.user}-orderless-literal-dispatcher
                    ${config.user}-orderless-without-literal-dispatcher
                    ${config.user}-orderless-initialism-dispatcher
                    ${config.user}-orderless-flex-dispatcher))

            (setq completion-styles '(orderless basic))
            (setq completion-category-overrides
                  '((project-file (styles orderless partial-completion basic))
                    (file (styles orderless partial-completion basic))))
            (setq enable-recursive-minibuffers t))

          (setq history-length 10000)
          (setq savehist-file
                (concat (or (getenv "XDG_CACHE_HOME") "~/.cache") "/emacs/history"))
          
          (define-key global-map (kbd "s-.") 'embark-act)
          (define-key global-map (kbd "s->") 'embark-become)
          (define-key minibuffer-local-map (kbd "M-r") 'consult-history)
          (define-key global-map (kbd "M-y") 'consult-yank-pop)
          (define-key global-map (kbd "s-B") 'consult-buffer)
          (define-key global-map (kbd "C-x C-r") 'consult-recent-file)
          (define-key minibuffer-local-map (kbd "s-g") 'embark-become)
          
          (let ((map goto-map))
            (define-key map (kbd "g") 'consult-goto-line)
            (define-key map (kbd "M-g") 'consult-goto-line)
            (define-key map (kbd "l") 'consult-line)
            (define-key map (kbd "o") 'consult-outline)
            (define-key map (kbd "i") 'consult-imenu)
            (define-key map (kbd "m") 'consult-mark)
            (define-key map (kbd "M") 'consult-global-mark)
            (define-key map (kbd "b") 'consult-bookmark))

          (defun ${config.user}-goto-line-relative ()
            "Just a wrapper around `consult-goto-line', which uses
            relative line numbers, when narrowing is active."
            (interactive)
            (let ((consult-line-numbers-widen nil))
              (call-interactively 'consult-goto-line)))
          
          (define-key narrow-map (kbd "g") '${config.user}-goto-line-relative)
          
          (let ((map search-map))
            (define-key map (kbd "f") 'consult-find)
            (define-key map (kbd "g") 'consult-ripgrep)
            (define-key map (kbd "e") 'consult-isearch-history)
            (define-key map (kbd "l") 'consult-line))
          
          (autoload 'consult-isearch-history "consult")
          
          (let ((map isearch-mode-map))
            (define-key map (kbd "M-e") 'consult-isearch-history)
            (define-key map (kbd "M-s e") 'consult-isearch-history)
            (define-key map (kbd "M-s l") 'consult-line))

          (define-key minibuffer-local-map (kbd "s-b") 'exit-minibuffer)
          (autoload 'consult-customize "consult" "" nil 'macro)
          (autoload 'consult--customize-set "consult")
          (autoload 'embark-open-externally "embark")

          (with-eval-after-load 'embark (require 'embark-consult))
          (with-eval-after-load 'xref (setq xref-show-xrefs-function 'consult-xref))
          (with-eval-after-load
            'consult
            (require 'embark-consult)
            (consult-customize consult-buffer :preview-key "M-.")
            (consult-customize consult-history :category 'consult-history)
            (consult-customize consult-line :inherit-input-method t))
            
          (with-eval-after-load 'marginalia (setq marginalia-align 'right))
          (autoload 'marginalia-mode "marginalia")
          (marginalia-mode 1)
        '';
      };
    };
  };
}
