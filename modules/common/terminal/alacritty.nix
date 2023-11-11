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
        draw_bold_text_with_bright_colors = true;

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
	mouse_bindings = [
	  { mouse = "Middle"; action = "PasteSelection"; }
	];

	## Time in ms to wait
        mouse = {
          double_click = { threshold = 300; };
          triple_click = { threshold = 300; };

	  ## Don't hide the cursor while typing
          hide_then_typing = false;

          ## Open urls without modifier
          hints.modifiers = true;
        };

        selection = {
          semantic_escape_chars = ",│`|:\"' ()[]{}<>";
          save_to_clipboard = true;
        };

	## Allow applications to change window title
        dynamic_tile = false;
        
        cursor = {
          style = "Block";
          unfocused_hollow = true;
        };

        live_config_reload = true;

	## No predefined PWD. It'll use dir of parent process
	working_directory = "None";

	## Windows only
        enable_experimental_conpty_backend = false;

	## Key bindings
	key_bindings = [
	   { key = "Paste";                   		action = "Paste";                           	}
	   { key = "Copy";                    		action = "Copy";                            	}
	   { key = "L";        mods = "Control"; 	action = "ClearLogNotice";                  	}
	   { key = "L";        mods = "Control"; 	chars = "\\x0c";                           	}
	   { key = "Home";     mods = "Alt";     	chars = "\\x1b[1;3H";                      	}
	   { key = "Home";                    		chars = "\\x1bOH";        mode = "AppCursor"; 	}
	   { key = "Home";                    		chars = "\\x1b[H";        mode = "~AppCursor";	}
	   { key = "End";      mods = "Alt";     	chars = "\\x1b[1;3F";                      	}
	   { key = "End";                     		chars = "\\x1bOF";        mode = "AppCursor"; 	}
	   { key = "End";                     		chars = "\\x1b[F";        mode = "~AppCursor";	}
	   { key = "PageUp";   mods = "Shift";   	action = "ScrollPageUp";   mode = "~Alt";      	}
	   { key = "PageUp";   mods = "Shift";   	chars = "\\x1b[5;2~";     mode = "Alt";       	}
	   { key = "PageUp";   mods = "Control"; 	chars = "\\x1b[5;5~";                      	}
	   { key = "PageUp";   mods = "Alt";     	chars = "\\x1b[5;3~";                      	}
	   { key = "PageUp";                  		chars = "\\x1b[5~";                        	}
	   { key = "PageDown"; mods = "Shift";   	action = "ScrollPageDown"; mode = "~Alt";      	}
	   { key = "PageDown"; mods = "Shift";   	chars = "\\x1b[6;2~";     mode = "Alt";       	}
	   { key = "PageDown"; mods = "Control"; 	chars = "\\x1b[6;5~";                      	}
	   { key = "PageDown"; mods = "Alt";     	chars = "\\x1b[6;3~";                      	}
	   { key = "PageDown";                		chars = "\\x1b[6~";                        	}
	   { key = "Tab";      mods = "Shift";   	chars = "\\x1b[Z";                         	}
	   { key = "Back";                    		chars = "\\x7f";                           	}
	   { key = "Back";     mods = "Alt";     	chars = "\\x1b\\x7f";                       	}
	   { key = "Insert";                  		chars = "\\x1b[2~";                        	}
	   { key = "Delete";                  		chars = "\\x1b[3~";                        	}
	   { key = "Left";     mods = "Shift";   	chars = "\\x1b[1;2D";                      	}
	   { key = "Left";     mods = "Control"; 	chars = "\\x1b[1;5D";                      	}
	   { key = "Left";     mods = "Alt";     	chars = "\\x1b[1;3D";                      	}
	   { key = "Left";                    		chars = "\\x1b[D";        mode = "~AppCursor";	}
	   { key = "Left";                    		chars = "\\x1bOD";        mode = "AppCursor"; 	}
	   { key = "Right";    mods = "Shift";   	chars = "\\x1b[1;2C";                      	}
	   { key = "Right";    mods = "Control"; 	chars = "\\x1b[1;5C";                      	}
	   { key = "Right";    mods = "Alt";     	chars = "\\x1b[1;3C";                      	}
	   { key = "Right";                   		chars = "\\x1b[C";        mode = "~AppCursor";	}
	   { key = "Right";                   		chars = "\\x1bOC";        mode = "AppCursor"; 	}
	   { key = "Up";       mods = "Shift";   	chars = "\\x1b[1;2A";                      	}
	   { key = "Up";       mods = "Control"; 	chars = "\\x1b[1;5A";                      	}
	   { key = "Up";       mods = "Alt";     	chars = "\\x1b[1;3A";                      	}
	   { key = "Up";                      		chars = "\\x1b[A";        mode = "~AppCursor";	}
	   { key = "Up";                      		chars = "\\x1bOA";        mode = "AppCursor"; 	}
	   { key = "Down";     mods = "Shift";   	chars = "\\x1b[1;2B";                      	}
	   { key = "Down";     mods = "Control"; 	chars = "\\x1b[1;5B";                      	}
	   { key = "Down";     mods = "Alt";     	chars = "\\x1b[1;3B";                      	}
	   { key = "Down";                    		chars = "\\x1b[B";        mode = "~AppCursor";	}
	   { key = "Down";                    		chars = "\\x1bOB";        mode = "AppCursor"; 	}
	   { key = "F1";                      		chars = "\\x1bOP";                         	}
	   { key = "F2";                      		chars = "\\x1bOQ";                         	}
	   { key = "F3";                      		chars = "\\x1bOR";                         	}
	   { key = "F4";                      		chars = "\\x1bOS";                         	}
	   { key = "F5";                      		chars = "\\x1b[15~";                       	}
	   { key = "F6";                      		chars = "\\x1b[17~";                       	}
	   { key = "F7";                      		chars = "\\x1b[18~";                       	}
	   { key = "F8";                      		chars = "\\x1b[19~";                       	}
	   { key = "F9";                      		chars = "\\x1b[20~";                       	}
	   { key = "F10";                     		chars = "\\x1b[21~";                       	}
	   { key = "F11";                     		chars = "\\x1b[23~";                       	}
	   { key = "F12";                     		chars = "\\x1b[24~";                       	}
	   { key = "F1";       mods = "Shift";   	chars = "\\x1b[1;2P";                      	}
	   { key = "F2";       mods = "Shift";   	chars = "\\x1b[1;2Q";                      	}
	   { key = "F3";       mods = "Shift";   	chars = "\\x1b[1;2R";                      	}
	   { key = "F4";       mods = "Shift";   	chars = "\\x1b[1;2S";                      	}
	   { key = "F5";       mods = "Shift";   	chars = "\\x1b[15;2~";                     	}
	   { key = "F6";       mods = "Shift";   	chars = "\\x1b[17;2~";                     	}
	   { key = "F7";       mods = "Shift";   	chars = "\\x1b[18;2~";                     	}
	   { key = "F8";       mods = "Shift";   	chars = "\\x1b[19;2~";                     	}
	   { key = "F9";       mods = "Shift";   	chars = "\\x1b[20;2~";                     	}
	   { key = "F10";      mods = "Shift";   	chars = "\\x1b[21;2~";                     	}
	   { key = "F11";      mods = "Shift";   	chars = "\\x1b[23;2~";                     	}
	   { key = "F12";      mods = "Shift";   	chars = "\\x1b[24;2~";                     	}
	   { key = "F1";       mods = "Control"; 	chars = "\\x1b[1;5P";                      	}
	   { key = "F2";       mods = "Control"; 	chars = "\\x1b[1;5Q";                      	}
	   { key = "F3";       mods = "Control"; 	chars = "\\x1b[1;5R";                      	}
	   { key = "F4";       mods = "Control"; 	chars = "\\x1b[1;5S";                      	}
	   { key = "F5";       mods = "Control"; 	chars = "\\x1b[15;5~";                     	}
	   { key = "F6";       mods = "Control"; 	chars = "\\x1b[17;5~";                     	}
	   { key = "F7";       mods = "Control"; 	chars = "\\x1b[18;5~";                     	}
	   { key = "F8";       mods = "Control"; 	chars = "\\x1b[19;5~";                     	}
	   { key = "F9";       mods = "Control"; 	chars = "\\x1b[20;5~";                     	}
	   { key = "F10";      mods = "Control"; 	chars = "\\x1b[21;5~";                     	}
	   { key = "F11";      mods = "Control"; 	chars = "\\x1b[23;5~";                     	}
	   { key = "F12";      mods = "Control";	chars = "\\x1b[24;5~";                     	}
	   { key = "F1";       mods = "Alt";     	chars = "\\x1b[1;6P";                      	}
	   { key = "F2";       mods = "Alt";     	chars = "\\x1b[1;6Q";                      	}
	   { key = "F3";       mods = "Alt";     	chars = "\\x1b[1;6R";                      	}
	   { key = "F4";       mods = "Alt";     	chars = "\\x1b[1;6S";                      	}
	   { key = "F5";       mods = "Alt";     	chars = "\\x1b[15;6~";                     	}
	   { key = "F6";       mods = "Alt";     	chars = "\\x1b[17;6~";                     	}
	   { key = "F7";       mods = "Alt";     	chars = "\\x1b[18;6~";                     	}
	   { key = "F8";       mods = "Alt";     	chars = "\\x1b[19;6~";                     	}
	   { key = "F9";       mods = "Alt";     	chars = "\\x1b[20;6~";                     	}
	   { key = "F10";      mods = "Alt";     	chars = "\\x1b[21;6~";                     	}
	   { key = "F11";      mods = "Alt";     	chars = "\\x1b[23;6~";                     	}
	   { key = "F12";      mods = "Alt";     	chars = "\\x1b[24;6~";                     	}
	   { key = "F1";       mods = "Super";   	chars = "\\x1b[1;3P";                      	}
	   { key = "F2";       mods = "Super";   	chars = "\\x1b[1;3Q";                      	}
	   { key = "F3";       mods = "Super";   	chars = "\\x1b[1;3R";                      	}
	   { key = "F4";       mods = "Super";   	chars = "\\x1b[1;3S";                      	}
	   { key = "F5";       mods = "Super";   	chars = "\\x1b[15;3~";                     	}
	   { key = "F6";       mods = "Super";   	chars = "\\x1b[17;3~";                     	}
	   { key = "F7";       mods = "Super";   	chars = "\\x1b[18;3~";                     	}
	   { key = "F8";       mods = "Super";   	chars = "\\x1b[19;3~";                     	}
	   { key = "F9";       mods = "Super";   	chars = "\\x1b[20;3~";                     	}
	   { key = "F10";      mods = "Super";   	chars = "\\x1b[21;3~";                     	}
	   { key = "F11";      mods = "Super";   	chars = "\\x1b[23;3~";                     	}
	   { key = "F12";      mods = "Super";   	chars = "\\x1b[24;3~";                     	}
	   { key = "NumpadEnter";             		chars = "\\n";                             	}
        ];
      };
    };
  };
  };
}
