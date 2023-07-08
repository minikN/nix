
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
### Emacs configuration
###
### CODE:

{ config, lib, pkgs, inputs, ... }:


  let
    #nurNoPkgs = import config.inputs.nur { pkgs = null; nurpkgs = pkgs; };
  in {
  imports = [
    ./appearance.nix
  ];
  
  config = {
    home-manager.users.${config.user} = {
      #imports = [
      #  nur-no-pkgs.repos.rycee.hmModules.emacs-init
      #];
      services.emacs = {
        enable = true;
        package = pkgs.emacs29-pgtk;
      };
      

      programs.emacs = {
        enable = true;
        package = pkgs.emacs29-pgtk;
        extraPackages = epkgs: [ epkgs.magit ];
        extraConfig = ''
          ;; Packages will be initialized by guix later.
          (setq package-enable-at-startup nil)
          (setq package-archives nil)

          ;; Defer garbage collection further back in the startup process
          (setq gc-cons-threshold most-positive-fixnum
                gc-cons-percentage 0.6)

          (add-hook 'emacs-startup-hook
            (lambda ()
              (setq undo-limit (* 8 1024 1024)
	                  read-process-output-max (* 1024 1024))))

          ;; Ignore X resources
          (advice-add #'x-apply-session-resources :override #'ignore)
          ;; TODO: Probably the better approach is:
          ;; (setq inhibit-x-resources t)

          ;; Do not resize the frame at this early stage.
          (setq frame-inhibit-implied-resize t)
          
          (pixel-scroll-precision-mode 1)

          ;; Set some defaults for the startup behaviour.
          (setq inhibit-splash-screen t
                inhibit-startup-screen t
                inhibit-startup-buffer-menu t
                inhibit-startup-echo-area-message "Welcome."
                use-dialog-box t
                use-file-dialog nil)

          ;; Theme
          (load-theme 'modus-operandi t)

          ;; Font
          (add-to-list 'default-frame-alist '(font . "${config.os.fonts.mono.regular}-${builtins.toString config.os.fonts.mono.size}"))
          (set-face-attribute 'default t :font "${config.os.fonts.mono.regular}-${builtins.toString config.os.fonts.mono.size}")
        '';
      };
    };
  };
  }


