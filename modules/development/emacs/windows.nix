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
  config = {
    home-manager.users.${config.user} = {
      programs.emacs = {
        extraConfig = ''
          (if after-init-time
            (winner-mode 1)
            (add-hook 'after-init-hook 'winner-mode))

          (defcustom ${config.user}-window-right-regex
            "\\*\\(?:help\\|grep\\|Completions\\)\\*"
            "Regex string matching buffers being shown in right side window")
          
          (defcustom ${config.user}-window-bottom-regex
                      "\\*\\(?:shell\\|compilation\\)\\*"
                      "Regex string matching buffers being shown in bottom side window")
          
            (defvar parameters
                '(window-parameters . ((no-other-window . t)
                                      (no-delete-other-windows . t))))

            (setq fit-window-to-buffer-horizontally t)
            (setq window-resize-pixelwise t)
            
            (setq display-buffer-alist
                  `((,${config.user}-window-right-regex display-buffer-in-side-window
                      (side . right) (slot . 0) (window-width . 50)
                      (preserve-size . (t . nil)) ,parameters)
                    (,${config.user}-window-bottom-regex display-buffer-in-side-window
                      (side . bottom) (slot . 1) (preserve-size . (nil . t))
                      ,parameters)))

           (defun ${config.user}--get-with-matching-buffer (target regex list)
             "Returns the first TARGET that has (or is) a buffer
           matching REGEX in LIST"
             (car (seq-filter
             (lambda (x)
               (string-match-p
               regex
               (buffer-name
                 (cond ((equal target "window") (window-buffer x))
                 ((equal target "buffer") x)))))
             list)))

           (defun ${config.user}--get-matching-buffer (regex)
             "Get the buffer matching REGEX"
             (${config.user}--get-with-matching-buffer
             "buffer"
             regex
             (buffer-list)))

           (defun ${config.user}--get-matching-window (regex)
             "Get the window with a buffer matching REGEX"
             (${config.user}--get-with-matching-buffer
             "window"
             regex
             (window-list-1 nil 0 t)))

           (defun ${config.user}-window-delete-side-window (regex)
             "Deletes window with a buffer matching REGEX"
             (delete-window (${config.user}--get-matching-window regex)))
           (defun ${config.user}-window-show-side-window (regex)
             "Shows a side window with a buffer matching REGEX"
             (let ((matching-buffer (${config.user}--get-matching-buffer regex)))
               (when matching-buffer
                 (display-buffer matching-buffer))))

           (defun ${config.user}--is-side-window-visible-p (regex)
             "Returns the name of the window a buffer matching REGEX is currently
                     being displayed or `nil' if the window is not visible"
             (let ((matching-buffer (${config.user}--get-matching-buffer regex)))
               (get-buffer-window (or matching-buffer ""))))

           (defun ${config.user}-toggle-side-window (regex)
             "Toggles side window having buffers matching REGEX"
             (if (${config.user}--is-side-window-visible-p regex)
                 (${config.user}-window-delete-side-window regex)
               (${config.user}-window-show-side-window regex)))

           (defun ${config.user}--is-side-window-selected-p (regex)
             (equal (selected-window) (${config.user}--get-matching-window regex)))
          
          (defun ${config.user}-focus-window (regex)
            "Focuses window that has buffers matching REGEX by deleting all other windows.
          Will restore previous window layout on subsequent execution. NOTE: Layout changes
          during the target window is focused will be lost once the layout is restored."
            (let ((target (${config.user}--get-matching-window regex))
            (ignore-window-parameters t))
              (cond (;; Saving current window layout and deleting all windows other than TARGET
              ;; if TARGET is live and not the only window
              (and (> (count-windows) 1) (window-live-p target))
              (set-frame-parameter
                (window-frame) 'window-state (window-state-get (frame-root-window (window-frame))))
              (delete-other-windows target))
              (;; Stolen from window.el
              ;; Recovering old window layout
              (setq state (frame-parameter (window-frame) 'window-state))
              (let ((window-combination-resize t)
              (main-state (window-state-get (frame-root-window (window-frame)))))
                      (window-state-put state (frame-root-window (window-frame)) t))
              (window--sides-reverse-frame (window-frame))))))

          ;; toggling right side window
          (global-set-key (kbd "<f11>") (lambda ()
                                           (interactive)
                                           (${config.user}-toggle-side-window
                                           ${config.user}-window-right-regex)))
          
          ;; focusing right side window
          (global-set-key (kbd "S-<f11>") (lambda ()
                                           (interactive)
                                           (${config.user}-focus-window
                                           ${config.user}-window-right-regex)))
          
           ;; toggling bottom side window
           (global-set-key (kbd "<f10>") (lambda ()
                                           (interactive)
                                           (${config.user}-toggle-side-window
                                           ${config.user}-window-bottom-regex)))
           
           ;; focusing bottom side window
           (global-set-key (kbd "S-<f10>") (lambda ()
                                           (interactive)
                                           (${config.user}-focus-window
                                           ${config.user}-window-bottom-regex)))


          (global-set-key (kbd "<f12>") (lambda () (interactive) (${config.user}-toggle-window ${config.user}-window-right-regex)))   

        '';
      };
    };
  };
}

          # (defun ${config.user}-toggle-focus-side-window (regex &optional horizontal)
          #   "Toggles the focus on a side window."
          #   (if (${config.user}--is-side-window-selected-p regex)
          #       (progn
          #         (shrink-window 50 (or horizontal nil))
          #         (other-window 1))
          #       (let ((window (${config.user}--get-matching-window regex)))
          #         (when window
          #           (select-window window)
          #           (enlarge-window 50 (or horizontal nil))))))

          # ;; focusing right side window
          # (global-set-key (kbd "S-<f11>") (lambda ()
          #                                   (interactive)
          #                                   (${config.user}-toggle-focus-side-window
          #                                   ${config.user}-window-right-regex
          #                                   t)))
          
          # ;; focusing bottom side window
          # (global-set-key (kbd "S-<f10>") (lambda ()
          #                                   (interactive)
          #                                   (${config.user}-toggle-focus-side-window
          #                                   ${config.user}-window-bottom-regex)))