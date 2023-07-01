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
### Bootloader configuration
###
### CODE:

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

