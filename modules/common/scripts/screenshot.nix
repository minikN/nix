
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
### Screenshot script configuration
###
### CODE:

{ config, lib, pkgs, ... }:

{
  options = {
    os = {
      screenshot = lib.mkOption {
        type = lib.types.path;
        description = "Utility for taking screenshots";
      };
    };
  };

  config = let
    screenshot = if config.os.wm == "sway"
      then
        ## TODO: Make this accept an exernal file and substitute all dependencies
        pkgs.writeShellScriptBin "screenshot" ''
          notify () {
            if [ -x "$(command -v ${pkgs.libnotify}/bin/notify-send)" ]; then
              ${pkgs.libnotify}/bin/notify-send -t 5000 "$1" "$2" 
            fi
          }

          typeChoice=$(echo -e "Region\nAll outputs\nSpecific output\nSpecific window\nActive output\nActive window" | ${config.os.launcher.pkg}/bin/${config.os.launcher.name} -dmenu -i -fuzzy -p "Take screenshot of")
          editChoice=$(echo -e "Edit\nClipboard\nSave" | ${config.os.launcher.pkg}/bin/${config.os.launcher.name} -dmenu -i -fuzzy -p "What to do with screenshot")

          filename="$XDG_PICTURES_DIR/screenshots/$(date +'screenshot_%s.png')"

          case $editChoice in

              "Clipboard")
                  [[ "$typeChoice" == "Region" ]] && ${pkgs.grim}/bin/grim -g "$(${pkgs.slurp}/bin/slurp)" - | ${pkgs.wl-clipboard}/bin/wl-copy -t "image/png"
                  [[ "$typeChoice" == "All outputs" ]] && ${pkgs.grim}/bin/grim - | ${pkgs.wl-clipboard}/bin/wl-copy -t "image/png"
                  [[ "$typeChoice" == "Specific output" ]] && ${pkgs.grim}/bin/grim -o $(${pkgs.slurp}/bin/slurp -o -f "%o") - | ${pkgs.wl-clipboard}/bin/wl-copy -t "image/png"
                  [[ "$typeChoice" == "Active output" ]] && ${pkgs.grim}/bin/grim -o $(${pkgs.sway}/bin/swaymsg -t get_outputs | ${pkgs.jq}/bin/jq -r '.[] | select(.focused) | .name') - | ${pkgs.wl-clipboard}/bin/wl-copy -t "image/png"
                  [[ "$typeChoice" == "Active window" ]] && ${pkgs.grim}/bin/grim -g "$(${pkgs.sway}/bin/swaymsg -t get_tree | ${pkgs.jq}/bin/jq -j '.. | select(.type?) | select(.focused).rect | "\(.x),\(.y) \(.width)x\(.height)"')" - | ${pkgs.wl-clipboard}/bin/wl-copy -t "image/png"
                  [[ "$typeChoice" == "Specific window" ]] && ${pkgs.grim}/bin/grim -g "$(${pkgs.sway}/bin/swaymsg -t get_tree | ${pkgs.jq}/bin/jq -r '.. | select(.pid? and .visible?) | .rect | "\(.x),\(.y) \(.width)x\(.height)"' | ${pkgs.slurp}/bin/slurp)" - | ${pkgs.wl-clipboard}/bin/wl-copy -t "image/png"
                  notify "Screenshot saved to clipboard."
                  ;;

              "Edit")
                  [[ "$typeChoice" == "Region" ]] && ${pkgs.grim}/bin/grim -g "$(${pkgs.slurp}/bin/slurp)" - | ${pkgs.swappy}/bin/swappy -f -
                  [[ "$typeChoice" == "All outputs" ]] && ${pkgs.grim}/bin/grim - | ${pkgs.swappy}/bin/swappy -f -
                  [[ "$typeChoice" == "Specific output" ]] && ${pkgs.grim}/bin/grim -o $(${pkgs.slurp}/bin/slurp -o -f "%o") - | ${pkgs.swappy}/bin/swappy -f -
                  [[ "$typeChoice" == "Active output" ]] && ${pkgs.grim}/bin/grim -o $(${pkgs.sway}/bin/swaymsg -t get_outputs | ${pkgs.jq}/bin/jq -r '.[] | select(.focused) | .name') - | ${pkgs.swappy}/bin/swappy -f -
                  [[ "$typeChoice" == "Active window" ]] && ${pkgs.grim}/bin/grim -g "$(${pkgs.sway}/bin/swaymsg -t get_tree | ${pkgs.jq}/bin/jq -j '.. | select(.type?) | select(.focused).rect | "\(.x),\(.y) \(.width)x\(.height)"')" - | ${pkgs.swappy}/bin/swappy -f -
                  [[ "$typeChoice" == "Specific window" ]] && ${pkgs.grim}/bin/grim -g "$(${pkgs.sway}/bin/swaymsg -t get_tree | ${pkgs.jq}/bin/jq -r '.. | select(.pid? and .visible?) | .rect | "\(.x),\(.y) \(.width)x\(.height)"' | ${pkgs.slurp}/bin/slurp)" - | ${pkgs.swappy}/bin/swappy -f -
                  ;;

              "Save")
                  [[ "$typeChoice" == "Region" ]] && ${pkgs.grim}/bin/grim -g "$(${pkgs.slurp}/bin/slurp)" "$filename"
                  [[ "$typeChoice" == "All outputs" ]] && ${pkgs.grim}/bin/grim "$filename"
                  [[ "$typeChoice" == "Specific output" ]] && ${pkgs.grim}/bin/grim -o $(${pkgs.slurp}/bin/slurp -o -f "%o") "$filename"
                  [[ "$typeChoice" == "Active output" ]] && ${pkgs.grim}/bin/grim -o $(${pkgs.sway}/bin/swaymsg -t get_outputs | ${pkgs.jq}/bin/jq -r '.[] | select(.focused) | .name') "$filename"
                  [[ "$typeChoice" == "Active window" ]] && ${pkgs.grim}/bin/grim -g "$(${pkgs.sway}/bin/swaymsg -t get_tree | ${pkgs.jq}/bin/jq -j '.. | select(.type?) | select(.focused).rect | "\(.x),\(.y) \(.width)x\(.height)"')" "$filename"
                  [[ "$typeChoice" == "Specific window" ]] && ${pkgs.grim}/bin/grim -g "$(${pkgs.sway}/bin/swaymsg -t get_tree | ${pkgs.jq}/bin/jq -r '.. | select(.pid? and .visible?) | .rect | "\(.x),\(.y) \(.width)x\(.height)"' | ${pkgs.slurp}/bin/slurp)" "$filename"
                  notify "Screenshot saved." "$filename"
                  ;;
          esac
        ''
      else null;
  in {

    ## Setting the screenshot utility
    os.screenshot = screenshot;

    home-manager.users.${config.user} = {
      home.packages = [ screenshot ];
    };
  };
}

