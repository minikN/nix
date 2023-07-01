##
## Filesystem configuration
##
## Machine-specific filesystem shoudl be declared in their
## corresponding machine config.

{ config, lib, pkgs, ... }:

{
  ## Main partition
  fileSystems."/" = {
    device = "/dev/disk/by-label/SYSTEM";
    fsType = "btrfs";
  };

  ## Boot partition
  fileSystems."/boot" = {
    device = "/dev/disk/by-label/BOOT";
    fsType = "vfat";
  };

  ## Swap partition
  swapDevices = [
    { device = "/dev/disk/by-label/SWAP"; }
  ];
}
