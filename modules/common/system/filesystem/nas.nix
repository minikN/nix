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
