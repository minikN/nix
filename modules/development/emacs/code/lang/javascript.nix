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
### Emacs javascript configuration
### Source: https://github.com/abcdw/rde/blob/master/src/rde/features/emacs-xyz.scm#L154
###
### CODE:

{ config, lib, pkgs ? import <nixpkgs> {
  inherit system;
}, system ? builtins.currentSystem, inputs, ... }:

{ 
  config = let
    customNodePackages = import ../../../../../packages/node/default.nix {
      inherit pkgs system;
    };
  in {
    nixpkgs.overlays = [
      (self: super: {
        vscode-js-debug = super.callPackage ../../../../../packages/node/vscode-js-debug.nix { };
      })
    ];

    home-manager.users.${config.user} = {
      home.packages = [ pkgs.nodejs ];

      programs.emacs = {
        extraPackages = epkgs: [
          ## node / npm
          epkgs.npm-mode
          epkgs.nodejs-repl
          epkgs.jsdoc

          epkgs.flymake-eslint
          epkgs.eslint-fix

          epkgs.consult-eglot ## Move

          ## Needed for eldoc to show MD previews
          epkgs.markdown-mode
        ];
        extraConfig = ''
;; ~!emacs-lisp!~
;; Tell emacs to use treesitter modes
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))
(add-to-list 'major-mode-remap-alist `(javascript-mode . js-ts-mode))

;; Creating a derived-mode for jsx files, so that we can assign the
;; correct language-id to it in `eglot-server-programs'
(define-derived-mode jsx-ts-mode js-ts-mode "JavaScript[JSX]")
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . jsx-ts-mode))

;; jsdoc
(use-package jsdoc
  :config
  (defun db-javascript-jsdoc-or-code-actions ()
    "Inserts JSDoc at point if line matches `/**'.
Otherwise executes `eglot-code-actions' at given
point."
    (interactive)
    (let ((p (point)))
      (beginning-of-line)
      (if (looking-at-p "^[[:blank:]]*/\\*\\*$")
	  (progn
	    (kill-line)
	    (next-line)
	    (jsdoc)
	    (goto-char (search-backward-regexp "^/\\*\\*$"))
	    (next-line)
	    (end-of-line))
	(progn
	  (goto-char p)
	  (funcall 'eglot-code-actions p nil nil t)))))

  :custom
  (jsdoc-append-dash nil))

(with-eval-after-load
    'dape
  ;; Fixes freezing problem
  ;; TODO: Remove after fixed upstream.
  (cl-defmethod dape-handle-event
    (conn (_event (eql loadedSource)) body) nil)
  
  (setq dape-configs-adapter-dir (file-name-as-directory (concat user-emacs-directory "dape-debuggers")))
  (setq dape-configs-port 8123)

  (add-to-list 'dape-configs
	       `(js-debug-chrome
		 modes (js-mode js-ts-mode)
		 command "${pkgs.vscode-js-debug}/bin/dapDebugServer"
		 command-cwd ,(concat dape-configs-adapter-dir "js-debug")
		 command-args (,(format "%d" dape-configs-port))
		 port dape-configs-port
		 :name "DEBUG"
		 :userDataDir nil
		 :type "pwa-chrome"
		 :trace nil
		 :url ,(lambda ()
			 (read-string "Url: "
				      "http://localhost:3000"))
		 :webRoot ,(lambda ()
			     (read-string "Root: "
					  (funcall dape-cwd-fn))))))

(defun db--javascript-setup-electric-pairs-for-jsx-tsx ()
  (electric-pair-local-mode)
  (setq-local electric-pair-pairs
	      (append electric-pair-pairs
		      '((60 . 62)))) ;; <, >
  (setq-local electric-pair-text-pairs electric-pair-pairs))

;; Configure js-ts-mode
(setq js-indent-level 2
      js-chain-indent t)

;; npm-mode
;; TODO: Add which-key descriptions
;; TODO: Make npm-mode run with isolated npm runtime
(with-eval-after-load
    'npm-mode
  (fset 'npm-mode-command-keymap npm-mode-command-keymap)
  (define-key npm-mode-keymap (kbd "C-c n") '("npm" . npm-mode-command-keymap)))

;; nodejs-repl
(defun db--javascript-setup-nodejs-repl ()
  (defvar nodejs-repl-mode-command-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "e") '("Send last expression" . nodejs-repl-send-last-expression))
      (define-key map (kbd "j") '("Send line" . nodejs-repl-send-line))
      (define-key map (kbd "r") '("Send region" . nodejs-repl-send-region))
      (define-key map (kbd "C-c") '("Send buffer" . nodejs-repl-send-buffer))
      (define-key map (kbd "C-l") '("Load file" . nodejs-repl-load-file))
      (define-key map (kbd "C-z") '("Switch to REPL" . nodejs-repl-switch-to-repl))
      map))
  (fset 'nodejs-repl-mode-command-map nodejs-repl-mode-command-map)
  (define-key js-ts-mode-map (kbd "C-c r")
	      '("repl" . nodejs-repl-mode-command-map)))

(with-eval-after-load
    'nodejs-repl
  (setq nodejs-repl-command "${pkgs.nodejs}/bin/node"))

;; flymake-eslint
;; TODO: Supress no-unused-vars error from either eslint or tsserver
(with-eval-after-load
    'flymake-eslint
  (setq flymake-eslint-executable-name "${pkgs.nodePackages_latest.eslint}/bin/eslint"))

(with-eval-after-load
    'eslint-fix
  (setq eslint-fix-executable "${pkgs.nodePackages_latest.eslint}/bin/eslint"))

;; Configure eglot
(with-eval-after-load
    'eglot
  (add-to-list
   'eglot-server-programs
   '(((jsx-ts-mode :language-id "javascriptreact") ;; needs to come before js-ts-mode
      (js-ts-mode :language-id "javascript")
      (tsx-ts-mode :language-id "typescriptreact") ;; needs to come before typescrip-ts-mode
      (typescript-ts-mode :language-id "typescript")) . ("${pkgs.nodePackages.typescript-language-server}/bin/typescript-language-server" "--stdio"
      :initializationOptions
      (:tsserver (:path "${pkgs.nodePackages.typescript}/lib/node_modules/typescript/lib")))))

  (add-hook 'eglot-managed-mode-hook (lambda () (flymake-mode t))))

(dolist
    (hook
     '(js-ts-mode-hook
       typescript-ts-mode-hook
       tsx-ts-mode-hook))
  (add-hook hook
	    (lambda ()
	      ;; Don't use tabs
	      (setq indent-tabs-mode nil)

	      ;; Set up flymake correctly
	      (setq-local eglot-stay-out-of '(flymake))

	      (add-hook 'flymake-diagnostic-functions 'eglot-flymake-backend nil t)
	      (flymake-eslint-enable)
	      (local-set-key (kbd "C-M-S-p") #'eslint-fix)

	      (eglot-ensure)
	      (npm-mode)
	      (db--javascript-setup-nodejs-repl)
	      (db--javascript-setup-electric-pairs-for-jsx-tsx))))
        '';
      };
    };
  };
}
