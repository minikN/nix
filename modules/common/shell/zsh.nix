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
### ZSH configuration
###
### CODE:

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

