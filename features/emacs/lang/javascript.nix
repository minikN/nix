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

{ config, lib, pkgs, inputs, ... }:

{ 
  imports = [
    ../../../features
    ../eglot.nix
  ];

  config = {
    features.emacs.lang.javascript = true;

    home-manager.users.${config.user} = {
      home.packages = [
        pkgs.nodePackages.vscode-langservers-extracted
        pkgs.nodePackages.typescript-language-server
        pkgs.nodePackages.typescript
        #node
      ];

      programs.emacs = let
        dape = pkgs.emacsPackages.trivialBuild {
          pname = "dape";
	        version = "0.1";
          src = pkgs.fetchFromGitHub {
            owner = "svaante";
            repo = "dape";
            rev = "a9f68bf701bede7a1e96e9f38b1ed42ba1eeb994";
            sha256 = "sha256-arU7cGvsEdd0VPw5DD/q22rWXVz6Fr574x40orv1A4M=";
          }; 
        };
      in {
        extraPackages = epkgs: [ epkgs.consult-eglot epkgs.markdown-mode epkgs.corfu dape ];
        extraConfig = ''
          ;; Tell emacs to use treesitter modes
          (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
          (add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))
          (add-to-list 'major-mode-remap-alist `(javascript-mode . js-ts-mode))

	  ;; dape
	  (with-eval-after-load
	    'dape
	    (setq dape-configs-adapter-dir (file-name-as-directory (concat user-emacs-directory "dape-debuggers")))
	    (setq dape-configs-js-debug-version "v1.84.0")
            (setq dape-configs-port 8123)

	    (add-to-list 'dape-configs
             `(js-debug-chrome
               modes (js-mode js-ts-mode)
	       command "${pkgs.nodejs_18}/bin/node"
               command-cwd ,(concat dape-configs-adapter-dir "js-debug")
               command-args ("src/dapDebugServer.js" ,(format "%d" dape-configs-port))
               port dape-configs-port
               :type "pwa-chrome"
               :trace t
               :url ,(lambda ()
                       (read-string "Url: "
                                    "http://localhost:3000"))
               :webRoot dape-cwd-fn
               :outputCapture "console")))

            (defun snam-install-vscode-js-debug ()
              "Run installation procedure to install JS debugging support"
              (interactive)
              (mkdir snam-vscode-js-debug-dir t)
              (let ((default-directory (expand-file-name snam-vscode-js-debug-dir)))
                
                (vc-git-clone "https://github.com/microsoft/vscode-js-debug.git" "." nil)
                (message "git repository created")
                (call-process "${pkgs.nodejs_18}/bin/npm" nil "*snam-install*" t "install")
                (message "npm dependencies installed")
                (call-process "${pkgs.nodejs_18}/bin/npx" nil "*snam-install*" t "${pkgs.nodePackages.gulp}/bin/gulp" "dapDebugServer")
                (message "vscode-js-debug installed")))

          (defun db--javascript-setup-electric-pairs-for-jsx-tsx ()
            (electric-pair-local-mode)
            (setq-local electric-pair-pairs
                        (append electric-pair-pairs
                                '((60 . 62)))) ;; <, >
            (setq-local electric-pair-text-pairs electric-pair-pairs))

          ;; Configure js-ts-mode
          (setq js-indent-level 2
                js-chain-indent t)


          ;; Configure eglot
          (with-eval-after-load
           'eglot
           (add-to-list
            'eglot-server-programs
            '((javascript-mode
               typescript-ts-mode
               tsx-ts-mode) . ("${pkgs.nodePackages.typescript-language-server}/bin/typescript-language-server" "--stdio"))))

          (dolist
           (hook
            '(js-ts-mode-hook
              typescript-ts-mode-hook
              tsx-ts-mode-hook))
            (add-hook hook
                      (lambda ()
                       (eglot-ensure)
                       (db--javascript-setup-electric-pairs-for-jsx-tsx))))
        '';
      };
    };
  };
}
