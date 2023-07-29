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
 
      config = let
       concatOutputs = lib.lists.foldl (all: output: if builtins.stringLength all > 0 then "${all}\" \"${output.name}" else "${output.name}") "";
      in {

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
        ## For left/right configuration see module hardware/outputs
        output = {
          
          ## Primary
          "${config.os.output.primary.name}" = lib.mkIf (config.os.machine.isLaptop == true) {
            scale = if config.os.output.primary.hidpi
              then "1.5"
              else "1";
          };
        };

        ## Assigning workspaces to outputs
        workspaceOutputAssign = [
          { workspace = "1"; output = concatOutputs config.os.output.left; }
          { workspace = "2"; output = concatOutputs config.os.output.right; }
          { workspace = "3"; output = concatOutputs config.os.output.right; }
          { workspace = "4"; output = concatOutputs config.os.output.left; }
          { workspace = "5"; output = concatOutputs config.os.output.left; }
          { workspace = "6"; output = concatOutputs config.os.output.right; }
          { workspace = "7"; output = concatOutputs config.os.output.left; }
          { workspace = "8"; output = concatOutputs config.os.output.left; }
        ];

        ## Fonts
        fonts = {
          names = [ config.os.fonts.mono.regular ];
        };

        ## Terminal
        terminal = config.os.terminal;

        ## Appearance
        floating.border = 2;
        window.border = 0;
	      window.titlebar = false;
        floating.titlebar = false;
	      gaps.inner = 8;

	      ## Launcher
	      menu = "${config.os.launcher.pkg}/bin/${config.os.launcher.name} ${config.os.launcher.args}";

        ## We'll start the bar through dbus
        bars = [];

	      ## Keybindings
        keybindings = let
	        mod = config.home-manager.users.${config.user}.wayland.windowManager.sway.config.modifier;
	        menu = config.home-manager.users.${config.user}.wayland.windowManager.sway.config.menu;
          passwordManager = config.os.passwordManager;
          screenshot = config.os.screenshot;
        in lib.mkOptionDefault {
	        "${mod}+Shift+r" = "reload";
          "${mod}+Shift+q" = "kill";
          "${mod}+Shift+f" = "fullscreen";
          "${mod}+Ctrl+Space" = "focus mode_toggle";
	        "${mod}+Shift+d" = "exec ${menu}";
          "${mod}+Shift+p" = "exec ${passwordManager}";
          "Print" = "exec ${screenshot}/bin/screenshot";
	      };
      };
    };
  };
}

