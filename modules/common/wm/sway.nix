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
### Sway configuration
###
### CODE:

{ config, lib, pkgs, ... }:

{
  ## Setting appropriate options so other modules can use them.
  os.wayland = true;
  os.wm = "sway";

  ## Sway
  home-manager.users.${config.user} = {

   ## TODO: Move to own module 
   programs.rofi = {
      enable = true;
      package = pkgs.rofi-wayland;
    };

    wayland.windowManager.sway = {
      enable = true;

      ## Enable XWayland
      xwayland = true;
      
      ## Properly expose env to DBus
      systemd.enable = true;
      wrapperFeatures = {
        base = true;
        gtk = true;
      };

      extraSessionCommands = ''
        export SDL_VIDEODRIVER=wayland
        export QT_QPA_PLATFORM=wayland
        export QT_WAYLAND_DISABLE_WINDOWDECORATIONS=1
        export _JAVA_AWT_WM_NOPARENTING=1
      '';
 
      config = {

        ## Set modifier
        modifier = "Mod4";

	## use `--to-code' in keybindings
        bindkeysToCode = true;

        ## Keyboard
        input = {
          "*" = {
            xkb_layout = config.os.keyboard.layout;
            xkb_options = config.os.keyboard.options;
          };
        };

        ## Output configuration
        output = {
          
          ## Primary
          "${config.os.output.primary.name}" = {
            scale = if config.os.output.primary.hidpi
              then "2"
              else "1";
          };
        };
        ## Fonts
        fonts = {
          names = [ config.os.fonts.mono ];
        };

        ## Terminal
        terminal = config.os.terminal;

        ## Appearance
        floating.border = 0;
        window.border = 0;
	window.titlebar = false;
	gaps.inner = 8;

	## Launcher
	menu = "${pkgs.rofi-wayland}/bin/rofi -show drun";

	## Keybindings
        keybindings = let
	  mod = config.home-manager.users.${config.user}.wayland.windowManager.sway.config.modifier;
	  menu = config.home-manager.users.${config.user}.wayland.windowManager.sway.config.menu;
        in lib.mkOptionDefault {
	  "${mod}+Shift+r" = "reload";
          "${mod}+Shift+q" = "kill";
          "${mod}+Shift+f" = "fullscreen";
          "${mod}+Ctrl+Space" = "focus mode_toggle";
	  "${mod}+Shift+d" = "exec ${menu}";
	};
      };
    };
  };
}

