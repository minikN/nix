##
## ZSH configuration
##

{ config, lib, pkgs, ... }:

{
  ## Enable zsh system-wide
  programs.zsh.enable = true;

  ## add zsh to `/etc/shells'
  environment.shells = with pkgs; [ zsh ];

  ## Enable zsh for current user
  users.users.${config.user}.shell = pkgs.zsh;

  ## ZSH configuration
  home-manager.users.${config.user}.programs.zsh = {
    enable = true;
  };
}

