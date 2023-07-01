##
## Alacritty configuration
##

{ config, lib, pkgs, ... }:

{
	 
  config = {
  os.terminal = "${pkgs.alacritty}/bin/alacritty";  

  ## Alacritty
  home-manager.users.${config.user} = {
    programs.alacritty = {
      enable = true;
    };
  };
  };
}

