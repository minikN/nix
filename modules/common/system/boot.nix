##
## Bootloader configuration
##

{ config, lib, pkgs, ... }:

{
  boot.loader = {
    grub = {

      ## Use Grub as the bootloader
      enable = true;

      ## Enable EFI support
      efiSupport = true;

      ## Scan for other OS on the system
      useOSProber = true;

      ## Don't install grub, required for UEFI?
      device = "nodev";      
    };
    
    ## Allow bootloader to alter the UEFI
    efi.canTouchEfiVariables = true;
  };
}

