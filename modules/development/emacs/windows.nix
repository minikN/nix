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
  in {
    home-manager.users.${config.user} = {
      programs.emacs = utils.emacsPkg {
        name = "db-windows";
        description = "Window configuration";
        require = true;
        code = ''
;; ~!emacs-lisp!~
;; TODO: Add shrink (font-size function)
;; TODO: Add proper toggle-all-windows function
;; TODO: Add major-mode to "regex"
;; TODO: Add side-window width/height variable
(defgroup db-windows nil
  "Tweaks to the built-in Emacs window management."
  :group 'db)

(defcustom db-window-right-regex
  "\\*\\(?:help\\|grep\\|info\\|Completions\\|Buffer list\\|eglot doc\\|corfu doc:.*\\|eldoc\\)\\*"
  "Regex string matching buffers being shown in right side window"
  :type 'string
  :group 'db-windows)

(defcustom db-window-bottom-left-regex
  "\\*\\(?:shell\\|compilation\\|npm.*\\| docker compose .*\\)\\*"
  "Regex string matching buffers being shown in bottom side window"
  :type 'string
  :group 'db-windows)

(defcustom db-window-bottom-right-regex
  "\\*dape-\\(debug\\|repl\\|processes\\)\\*"
  "Regex string matching buffers being shown in bottom left side window"
  :type 'string
  :group 'db-windows)

(defcustom db-window-debug-top-regex
  "\\*\\(dape-info Scope .*\\)\\*"
  "Regex string matching buffers being shown in the top debug side window"
  :type 'string
  :group 'db-windows)

(defcustom db-window-debug-bottom-regex
  "\\*dape-info \\(Stack\\|Breakpoints\\)\\*"
  "Regex string matching buffers being shown in the bottom side window"
  :type 'string
  :group 'db-windows)


(defvar parameters
  '(window-parameters . ((no-other-window . t)
                         (no-delete-other-windows . t))))

(setq fit-window-to-buffer-horizontally t)
(setq window-resize-pixelwise t)

(setq display-buffer-alist
      ;; RIGHT
      `((,db-window-right-regex display-buffer-in-side-window 
				(side . right) (slot . -10) (window-width . 50)
				(preserve-size . (t . nil)) ,parameters)
	;; BOTTOM
	(,db-window-bottom-right-regex display-buffer-in-side-window
				       (side . bottom) (slot . 1) (preserve-size . (t . nil))
				       (even-window-sizes t)
				       ,parameters)
	(,db-window-bottom-left-regex display-buffer-in-side-window
				      (side . bottom) (slot . -1) (preserve-size . (t . nil))
				      (even-window-sizes t)
				      (window-width 0.5)
				      ,parameters)))

(defun db--get-with-matching-buffer (target regex list)
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

(defun db--get-matching-buffer (regex)
  "Get the buffer matching REGEX"
  (db--get-with-matching-buffer
   "buffer"
   regex
   (buffer-list)))

(defun db--get-matching-window (regex)
  "Get the window with a buffer matching REGEX"
  (db--get-with-matching-buffer
   "window"
   regex
   (window-list-1 nil 0 t)))

(defun db-window-delete-side-window (regex)
  "Deletes window with a buffer matching REGEX"
  (delete-window (db--get-matching-window regex)))
(defun db-window-show-side-window (regex)
  "Shows a side window with a buffer matching REGEX"
  (let ((matching-buffer (db--get-matching-buffer regex)))
    (when matching-buffer
      (display-buffer matching-buffer))))

(defun db--is-side-window-visible-p (regex)
  "Returns the name of the window a buffer matching REGEX is currently
                    being displayed or `nil' if the window is not visible"
  (let ((matching-buffer (db--get-matching-buffer regex)))
    (get-buffer-window (or matching-buffer ""))))

          ;;;###autoload
(defun db-toggle-window (regex)
  "Toggles side window having buffers matching REGEX"
  (if (db--is-side-window-visible-p regex)
      (db-window-delete-side-window regex)
    (db-window-show-side-window regex)))

(defun db--is-side-window-selected-p (regex)
  "Checks whether the currently selected window has buffers
            matching REGEX"
  (equal (selected-window) (db--get-matching-window regex)))

(defun db--compare-buffers (a b)
  "Predicate comparing buffer A to buffer B.
Used to sort buffers alphabetically in
`db-cycle-buffer-in-window'."
  (string< (buffer-name a) (buffer-name b)))

(defun db-cycle-buffer-in-window (regex)
  "Calls `db-toggle-window' if no buffer matching
REGEX is live, e.g. their window is not visible.
If it is, it will cycle through all buffers
matching REGEX in order."
  (let* ((window (db--get-matching-window regex))
	 ;; Get a alphabatically ordered list of buffers
	 (sorted-buffers (delete-dups (sort
				       (match-buffers regex)
				       'db--compare-buffers))))
    (if (not (window-live-p window))
     	(db-toggle-window regex)
      (progn
	;; rotate the list until the first element is the
	;; currently displayed buffer. Then display the next
	;; buffer in the list. That way order of cycling
	;; will always be the same
        (while (not (eq
		     (car sorted-buffers)
		     (window-buffer window)))
          (setq sorted-buffers (nconc
				(last sorted-buffers)
				(butlast sorted-buffers))))
	(set-window-buffer window (cadr sorted-buffers))))))

(db-cycle-buffer-in-window db-window-right-regex)

;;;###autoload
(defun db-focus-window (regex)
  "Focuses window that has buffers matching REGEX by deleting all other windows.
          Will restore previous window layout on subsequent execution. NOTE: Layout changes
          during the target window is focused will be lost once the layout is restored."
  (let ((target (db--get-matching-window regex))
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

;; toggle all side windows
(global-set-key (kbd "<f12>") 'window-toggle-side-windows)


;; toggling right side window
(global-set-key (kbd "<f11>") (lambda ()
                                (interactive)
                                (db-toggle-window
                                 db-window-right-regex)))

;; focusing right side window
(global-set-key (kbd "S-<f11>") (lambda ()
                                  (interactive)
                                  (db-focus-window
                                   db-window-right-regex)))
;; cycling through right side window
(global-set-key (kbd "C-<f11>") (lambda ()
                                  (interactive)
                                  (db-cycle-buffer-in-window
                                   db-window-right-regex)))

;; toggling bottom side window
(global-set-key (kbd "<f10>") (lambda ()
                                (interactive)
                                (db-toggle-window
                                 db-window-bottom-regex)))

;; focusing bottom side window
(global-set-key (kbd "S-<f10>") (lambda ()
                                  (interactive)
                                  (db-focus-window
                                   db-window-bottom-regex)))

;; cycling through bottom side window
(global-set-key (kbd "C-<f10>") (lambda ()
                                  (interactive)
                                  (db-cycle-buffer-in-window
                                   db-window-bottom-regex)))

        '';
      };
    };
  };
}
