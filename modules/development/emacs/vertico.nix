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
### Emacs vertico configuration
###
### CODE:

{ config, lib, pkgs, inputs, ... }:

{
  config = {
    home-manager.users.${config.user} = {
      programs.emacs = {
        extraPackages = epkgs: [ epkgs.vertico ];
        extraConfig = ''
          (eval-when-compile
            (require 'vertico)
            (require 'vertico-multiform))
          
          (with-eval-after-load
            'minibuffer
            (setq completion-in-region-function
                  (lambda (&rest args)
                    (apply (if vertico-mode
                            'consult-completion-in-region
                            'completion--in-region)
                          args))))
          
          (with-eval-after-load
            'vertico
            (advice-add 
              'vertico--format-candidate
              :around
              (lambda (orig cand prefix suffix index _start)
                (let ((cand (funcall orig cand prefix suffix index _start)))
                  (concat
                    (if (= vertico--index index)
                      (propertize "» " 'face 'vertico-current)
                      "  ")
                    cand))))

            (define-key global-map (kbd "s-s") 'vertico-repeat)
            (require 'vertico-repeat)
            (add-hook 'minibuffer-setup-hook 'vertico-repeat-save)
            (setq vertico-cycle t)
            (require 'vertico-directory)

            (defun ${config.user}-vertico-kill-region-dwim (&optional count)
              "The function kills region if mark is active, otherwise
              calls `vertico-directory-delete-word'.  Prefix argument can be used to
              kill a few words or directories."
              (interactive "p")
              (if (use-region-p)
                (kill-region (region-beginning) (region-end) 'region)
                (vertico-directory-delete-word count)))
            
            (define-key vertico-map (kbd "C-w") '${config.user}-vertico-kill-region-dwim)
            
            (defun ${config.user}--vertico-prepare-header-line ()
              "The same as `${config.user}--move-mode-line-to-header', but also increase
              vertico-count by 1 to show one more candidate, which is hidden
              otherwise because mode line is expected to be present by height
              calculation function for vertico buffer."
              (setq-local header-line-format mode-line-format)
              (setq-local mode-line-format nil))
            
            (advice-add 'vertico--setup :after '${config.user}--vertico-prepare-header-line)
            (setq vertico-multiform-categories
              '((consult-grep buffer)
                (imenu buffer)
                (buffer)
                (info-menu buffer)
                (consult-org-heading buffer)
                (consult-history buffer)
                (consult-lsp-symbols buffer)
                (consult-xref buffer)
                (embark-keybinding buffer)
                (consult-location buffer)))

            (setq vertico-multiform-commands
              '((telega-chat-with buffer)
                (magit:--author flat)
                (Info-goto-node buffer)
                (info-lookup-symbol buffer)
                (Info-follow-reference buffer)
                (consult-yank-pop buffer)))

            (autoload 'vertico-multiform-mode "vertico-multiform")
            (vertico-multiform-mode))

          (autoload 'vertico-mode "vertico")

          (if after-init-time
            (vertico-mode 1)
            (add-hook 'after-init-hook 'vertico-mode))
        '';
      };
    };
  };
}