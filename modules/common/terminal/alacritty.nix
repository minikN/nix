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
### Alacritty configuration
###
### CODE:

{ config, lib, pkgs, ... }:

{
	imports = [
    ../terminal
  ];

  config = {
    os.terminal = "${pkgs.alacritty}/bin/alacritty";  

    ## Alacritty
    home-manager.users.${config.user} = {
      programs.alacritty = {
        enable = true;

        ## Settings
        settings = {

          ## Window settings
          window = {
            
            ## Setting both to `0' will ignore this
            dimensions.columns = 0;
            dimensions.lines = 0;

            ## Add some padding to the borders
            padding.x = 10;
            padding.y = 5;

            ## Spread additional padding evenly
            dynamic_padding = false;

	          ## Decorations
            decorations = "full";
          };

          ## History
          scrolling = {
            history = 10000;
            
            ## How many lines to scroll on each tick
            multiplier = 3;
          };

          ## Font
          font = {
	  	      size = config.os.fonts.size;

            offset.x = 0; ## letter spacing
            offset.y = 0; ## line spacing

            glyph_offset.x = 0;
            glyph_offset.y = 0;

            normal.family = config.os.fonts.mono.regular;
            normal.style = "Regular";

            bold.family = config.os.fonts.mono.regular;
            bold.style = "Bold";

            italic.family = config.os.fonts.mono.regular;
            italic.style = "Italic"; 
          };

	        ## For folders in `ls' for example
          colors.draw_bold_text_with_bright_colors = true;

	        ## Colors
          colors = {
            primary = {
              background = "0xfafafa";
              foreground = "0x383a42";
            };

            cursor = {
              text = "0xfafafa";
              cursor = "0x383a42";
            };

            normal = {
	            black =   "0xfafafa";
	            red =     "0xca1243";
	            green =   "0x50a14f";
	            yellow =  "0xc18401";
	            blue =    "0x4078f2";
	            magenta = "0xa626a4";
	            cyan =    "0x0184bc";
	            white =   "0x383a42";
            };

            bright = {
	            black =   "0xa0a1a7";
	            red =     "0xca1243";
	            green =   "0x50a14f";
	            yellow =  "0xc18401";
	            blue =    "0x4078f2";
	            magenta = "0xa626a4";
	            cyan =    "0x0184bc";
	            white =   "0x090a0b";
            };

            indexed_colors = [
	            { index = 16; color = "0xd75f00"; }
	            { index = 17; color = "0x986801"; }
	            { index = 18; color = "0xf0f0f1"; }
	            { index = 19; color = "0xe5e5e6"; }
	            { index = 20; color = "0x696c77"; }
	            { index = 21; color = "0x202227"; }
            ];
          };

	        ## Mouse keybindings
	        mouse.bindings = [
	          { mouse = "Middle"; action = "PasteSelection"; }
	        ];

          selection = {
            semantic_escape_chars = ",│`|:\"' ()[]{}<>";
            save_to_clipboard = true;
          };
          
          cursor = {
            style = "Block";
            unfocused_hollow = true;
          };

          live_config_reload = true;

	        ## No predefined PWD. It'll use dir of parent process
	        working_directory = "None";
        };
      };
    };
  };
}
