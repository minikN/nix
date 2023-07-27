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
### Waybar configuration
###
### CODE:

{ config, lib, pkgs, ... }:

{
  config = let
    mapOutputs = lib.lists.imap0 (i: output: "${output.name}");
  in {
    nixpkgs.overlays = [
      (self: super: {
        waybar = super.waybar.overrideAttrs (oldAttrs: {
            mesonFlags = oldAttrs.mesonFlags ++ [ "-Dexperimental=true" ];
        });
      })
    ];

    os.bar = "${pkgs.waybar}/bin/waybar";

    ## Configuration
    home-manager.users.${config.user}.programs.waybar = {
      enable = true;

      ## SystemD integration
      systemd.enable = true;
      systemd.target = "sway-session.target";
      
      ## Settings
      settings = {
        primary = lib.mkIf (config.os.machine.isLaptop == true) {
          layer = "top";
          position = "top";
          name = "primary";
          output = config.os.output.primary.name;
          modules-left = [
            (
              if config.os.wm == "sway"
              then "sway/workspaces"
              else "wlr/workspaces"
            )
          ];
          modules-center = ["${config.os.wm}/window"];
          modules-right = [
            "disk#system"
            "cpu"
            "memory"
            "temperature"
            "battery"
            "tray"
            "clock"
          ];
          "sway/window" = lib.mkIf (config.os.wm == "sway") {
            max-length = 50;
          };
          "sway/workspaces" = lib.mkIf (config.os.wm == "sway") {
            format = "{icon}";
            on-click = "activate";
            all-outputs = true;
            disable-scroll = true;
            sort-by-number = true;
            format-icons = {
              "1" = "";
              "2" = "󱄅";
              "3" = "";
              "4" = "󰃯";
              "5" = "󰫔";
              "6" = "󱋊";
              "7" = lib.mkIf config.features.gaming "󰺶";
              "8" = "";
              "urgent" = "";
              "focused" = "";
              "default" = "";
            };
            persistent_workspaces = {
                "1" = []; 
                "2" = []; 
                "3" = []; 
                "4" = []; 
                "5" = []; 
                "6" = []; 
                "7" = lib.mkIf config.features.gaming [];
                "8" = [];
            };
          };
          "battery" = lib.mkIf (config.os.machine.isLaptop) {
            format = "{icon}";
            format-charging = "󱐋 {icon}";
            tooltip-format = "Current capacity: {capacity}%\n{timeTo}";
            states = {
              empty = 5;
              low = 15;
              half-low = 40;
              half = 60;
              high = 85;
              full = 100;
            };
            format-icons = [ "" "" "" "" "" "" ];
          };
          clock = {
            tooltip-format = "<big>{:%B %Y}</big>\n<tt><small>{calendar}</small></tt>";
            format = "{:%H:%M}";
            interval = 60;
          };
           temperature = {
            critical-threshold = 80;
            hwmon-path = config.os.machine.temperaturePath;
            format-critical = "<span color=\"#ab4642\"> {temperatureC}°C</span>";
            format = " {temperatureC}°C";
            format-icons = [ "" "" "" "" "" ];
            interval = 2;
          };
          memory = {
            interval = 10;
            format = "󰍛 {}%";
            tooltip-format = "{used:0.1f}G / {total:0.1f}G";
          };
          cpu = {
            interval = 2;
            format = "󰻠 {usage}%";
            max-length = 10;
          };
          "disk\#system" = {
            interval = 30;
            format = "󰋊 {percentage_used}%";
            tooltip-format = "{path}: {used} / {total}";
            path = "/";
          };
        };

        left = if (config.os.machine.isLaptop != true)
        then {
          layer = "top";
          position = "top";
          name = "left";
          output = mapOutputs config.os.output.left;
          modules-left = [
            (
              if config.os.wm == "sway"
              then "sway/workspaces"
              else "wlr/workspaces"
            )
          ];
          modules-center = ["${config.os.wm}/window"];
          "sway/window" = lib.mkIf (config.os.wm == "sway") {
            max-length = 50;
          };
          "sway/workspaces" = lib.mkIf (config.os.wm == "sway") {
            format = "{icon}";
            on-click = "activate";
            all-outputs = false;
            disable-scroll = true;
            sort-by-number = true;
            format-icons = {
              "1" = "";
              "2" = "󱄅";
              "3" = "";
              "4" = "󰃯";
              "5" = "󰫔";
              "6" = "󱋊";
              "7" = lib.mkIf config.features.gaming "󰺶";
              "8" = "";
              "urgent" = "";
              "focused" = "";
              "default" = "";
            };
            persistent_workspaces = {
                "1" = mapOutputs config.os.output.left; 
                "2" = mapOutputs config.os.output.right; 
                "3" = mapOutputs config.os.output.right; 
                "4" = mapOutputs config.os.output.left; 
                "5" = mapOutputs config.os.output.left; 
                "6" = mapOutputs config.os.output.right; 
                "7" = lib.mkIf config.features.gaming mapOutputs config.os.output.left;
                "8" = mapOutputs config.os.output.left;
            };
          };
        } else {
          layer = "top";
          position = "top";
          name = "left";
          output = mapOutputs config.os.output.left;
          modules-center = ["${config.os.wm}/window"];
          "sway/window" = lib.mkIf (config.os.wm == "sway") {
            max-length = 50;
          };
        };

        right = if (config.os.machine.isLaptop != true)
        then {
          layer = "top";
          position = "top";
          name = "right";
          output = mapOutputs config.os.output.right;
          modules-left = [
            (
              if config.os.wm == "sway"
              then "sway/workspaces"
              else "wlr/workspaces"
            )
          ];
          modules-center = ["${config.os.wm}/window"];
          modules-right = [
            "disk#system"
            "cpu"
            "memory"
            "temperature"
            "battery"
            "tray"
            "clock"
          ];
          "sway/workspaces" = lib.mkIf (config.os.wm == "sway") {
            format = "{icon}";
            on-click = "activate";
            all-outputs = false;
            disable-scroll = true;
            sort-by-number = true;
            format-icons = {
              "1" = "";
              "2" = "󱄅";
              "3" = "";
              "4" = "󰃯";
              "5" = "󰫔";
              "6" = "󱋊";
              "7" = lib.mkIf config.features.gaming "󰺶";
              "8" = "";
              "urgent" = "";
              "focused" = "";
              "default" = "";
            };
            persistent_workspaces = {
                "1" = mapOutputs config.os.output.left; 
                "2" = mapOutputs config.os.output.right; 
                "3" = mapOutputs config.os.output.right; 
                "4" = mapOutputs config.os.output.left; 
                "5" = mapOutputs config.os.output.left; 
                "6" = mapOutputs config.os.output.right; 
                "7" = lib.mkIf config.features.gaming mapOutputs config.os.output.left;
                "8" = mapOutputs config.os.output.left;
            };
          };
          "sway/window" = lib.mkIf (config.os.wm == "sway") {
            max-length = 50;
          };
          "battery" = lib.mkIf (config.os.machine.isLaptop) {
            format = "{icon}";
            format-charging = "󱐋 {icon}";
            tooltip-format = "Current capacity: {capacity}%\n{timeTo}";
            states = {
              empty = 5;
              low = 15;
              half-low = 40;
              half = 60;
              high = 85;
              full = 100;
            };
            format-icons = [ "" "" "" "" "" "" ];
          };
          clock = {
            tooltip-format = "<big>{:%B %Y}</big>\n<tt><small>{calendar}</small></tt>";
            format = "{:%H:%M}";
            interval = 60;
          };
           temperature = {
            critical-threshold = 80;
            hwmon-path = config.os.machine.temperaturePath;
            format-critical = "<span color=\"#ab4642\"> {temperatureC}°C</span>";
            format = " {temperatureC}°C";
            format-icons = [ "" "" "" "" "" ];
            interval = 2;
          };
          memory = {
            interval = 10;
            format = "󰍛 {}%";
            tooltip-format = "{used:0.1f}G / {total:0.1f}G";
          };
          cpu = {
            interval = 2;
            format = "󰻠 {usage}%";
            max-length = 10;
          };
          "disk\#system" = {
            interval = 30;
            format = "󰋊 {percentage_used}%";
            tooltip-format = "{path}: {used} / {total}";
            path = "/";
          };
        } else {
          layer = "top";
          position = "top";
          name = "right";
          output = mapOutputs config.os.output.right;
          modules-center = ["${config.os.wm}/window"];
          "sway/window" = lib.mkIf (config.os.wm == "sway") {
            max-length = 50;
          };
        };
      };

      ## Styles  
      style = ''
        /*
        *
        * Base16 Default Dark
        * Author: Chris Kempson (http://chriskempson.com)
        *
        */

        @define-color base00 #181818;
        @define-color base01 #282828;
        @define-color base02 #383838;
        @define-color base03 #585858;
        @define-color base04 #b8b8b8;
        @define-color base05 #d8d8d8;
        @define-color base06 #e8e8e8;
        @define-color base07 #f8f8f8;
        @define-color base08 #ab4642;
        @define-color base09 #dc9656;
        @define-color base0A #f7ca88;
        @define-color base0B #a1b56c;
        @define-color base0C #86c1b9;
        @define-color base0D #7cafc2;
        @define-color base0E #ba8baf;
        @define-color base0F #a16946;

        * {
            font-family: Symbols Nerd Font Mono, Iosevka Nerd Font;
            transition: none;
            box-shadow: none;
            text-shadow: none;
            min-height: 0;
        }

        tooltip {
            border: solid @base02;
            background: @base01;
            opacity: 0.9;
        }

        tooltip label {
            color: @base05;
            padding: 0;
        }

        #waybar {
            color: @base04;
            background: @base01;
        }

        .modules-right label,
        .modules-right image {
            margin: 0.4em 0.2em;
            padding: 0 0.4em;
            background: @base02;
            border-radius: 0.2em;
        }

        .modules-left {
            margin-left: 0.2em;
        }

        .modules-right {
            margin-right: 0.2em;
        }

        #workspaces button {
            background: none;
            border-radius: 0.2em;
            margin: 0.4em 0.4em;
            padding: 0.2em 0.2em;
            color: @base05;
        }

        #workspaces button:hover {
            background: none;
            border: none;
        }

        #workspaces button.focused {
            /*background: @base02;*/
            color: @base0A;
            font-weight: bold;
        }

        #workspaces button.urgent {
            color: @base08;
            font-weight: bold;
        }

        #tray menu {
            color: @base05;
            background: @base01;
            border: solid 1px;
            border-color: @base02;
        }

        #tray menu menuitem {
            padding-top: 0px;
            padding-bottom: 0px;
            margin-top: 0.1em;
            margin-bottom: 0em;
        }

        #tray menu menuitem:hover {
            background: none;
        }

        #tray menu separator {
            background: @base03;
            padding-top: 1px;
            margin-top: 0.2em;
            margin-bottom: 0.2em;
        }

        #battery.discharging.empty {
            color: @base08;
        }

        #battery.discharging.low {
            color: @base09;
        }

        #disk,
        #cpu,
        #memory,
        #temperature,
        #pulseaudio,
        #tray,
        #clock {
            margin-right: 5px;
        }
      '';
    };
  };
}

