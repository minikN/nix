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
          src = pkgs.fetchurl {
            url = "https://raw.githubusercontent.com/svaante/dape/master/dape.el";
            sha256 = "b90384c64940345ec316ec807ec90c02f5631ead4b786ae91e8c648b9ee2babb";
          }; 
        };
      in {
        extraPackages = epkgs: [ epkgs.consult-eglot epkgs.markdown-mode epkgs.corfu dape ];
        extraConfig = ''
          ;; Tell emacs to use treesitter modes
          (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
          (add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))
          (add-to-list 'major-mode-remap-alist `(javascript-mode . js-ts-mode))

          (with-eval-after-load
            'dape
            (setq snam-vscode-js-debug-dir (file-name-concat user-emacs-directory "dape/vscode-js-debug"))
            (add-to-list 'dape-configs
              `(vscode-js-node
                modes (js-mode js-ts-mode typescript-mode typescript-ts-mode)
                host "localhost"
                port 8123
                command "${pkgs.nodejs_18}/bin/node"
                command-cwd ,(file-name-concat snam-vscode-js-debug-dir "dist")
                command-args ("src/dapDebugServer.js" "8123")
                :type "pwa-node"
                :request "launch"
                :cwd dape-cwd-fn
                :program dape-find-file-buffer-default
                :outputCapture "console"
                :sourceMapRenames t
                :pauseForSourceMap nil
                :enableContentValidation t
                :autoAttachChildProcesses t
                :console "internalConsole"
                :killBehavior "forceful")))

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

                                       #"--tsserver-path" ${pkgs.nodePackages.typescript}/lib/node_modules/typescript/lib
                                       #"--stdio"))))

