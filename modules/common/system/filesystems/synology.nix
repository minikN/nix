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
### Synology NAS configuration
###
### Machine-specific filesystem should be declared in their corresponding
### machine config.
###
### CODE:

{ config, lib, pkgs, ... }:

{
  environment.systemPackages = [ pkgs.cifs-utils ];
  fileSystems."/mnt/nas/movies" = {
    device = "//192.168.178.26/movies";
    fsType = "cifs";
    options = let
      automount_opts = "x-systemd.automount,noauto,x-systemd.idle-timeout=60,x-systemd.device-timeout=5s,x-systemd.mount-timeout=5s";
    in ["${automount_opts},credentials=/etc/nixos/smb-credentials,uid=1000,gid=100"];
  };

  fileSystems."/mnt/nas/shows" = {
    device = "//192.168.178.26/shows";
    fsType = "cifs";
    options = let
      automount_opts = "x-systemd.automount,noauto,x-systemd.idle-timeout=60,x-systemd.device-timeout=5s,x-systemd.mount-timeout=5s";
    in ["${automount_opts},credentials=/etc/nixos/smb-credentials,uid=1000,gid=100"];
  };

  fileSystems."/mnt/nas/music" = {
    device = "//192.168.178.26/music";
    fsType = "cifs";
    options = let
      automount_opts = "x-systemd.automount,noauto,x-systemd.idle-timeout=60,x-systemd.device-timeout=5s,x-systemd.mount-timeout=5s";
    in ["${automount_opts},credentials=/etc/nixos/smb-credentials,uid=1000,gid=100"];
  };

  fileSystems."/mnt/nas/audiobooks" = {
    device = "//192.168.178.26/audiobooks";
    fsType = "cifs";
    options = let
      automount_opts = "x-systemd.automount,noauto,x-systemd.idle-timeout=60,x-systemd.device-timeout=5s,x-systemd.mount-timeout=5s";
    in ["${automount_opts},credentials=/etc/nixos/smb-credentials,uid=1000,gid=100"];
  };

  fileSystems."/mnt/nas/temp" = {
    device = "//192.168.178.26/Temp";
    fsType = "cifs";
    options = let
      automount_opts = "x-systemd.automount,noauto,x-systemd.idle-timeout=60,x-systemd.device-timeout=5s,x-systemd.mount-timeout=5s";
    in ["${automount_opts},credentials=/etc/nixos/smb-credentials,uid=1000,gid=100"];
  };
}
