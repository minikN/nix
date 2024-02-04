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
### `geekcave' configuration
###
### CODE:

{ inputs, globals, overlays, ... }:

with inputs;

nixpkgs.lib.nixosSystem {

  ## Setting system architecture.
  system = "x86_64-linux";

  ## Modules
  ##
  ## It takes an array of modules.
  modules = [

    ## Passing our recursive list will set the variables it contains
    ## config-wide as long as we declare them as options using `mkOption'.
    globals

    ## This module will return a `home-manager' object that can be used
    ## in other modules (including this one).
    home-manager.nixosModules.home-manager {
      nixpkgs.overlays = overlays;
    }

    ## This module will return a `nur' object that can be used to access
    ## NUR packages.
    nur.nixosModules.nur

    ## System specific
    ##
    ## Closure that returns the module containing configuration specific
    ## to this machine. In order to make it a function we need to wrap it
    ## in ().
    ({ lib, config, pkgs, ... }: {
      ## networking
      networking.hostName = "geekcave";
     
      ## kernel
      #boot.initrd.kernelModules = [ "amdgpu" ];
      boot.initrd.availableKernelModules = [
        "xhci_pci" "ahci" "usbhid"
        "usb_storage" "sd_mod"
      ];
      
      boot.kernelModules = [ "kvm-intel" ];
      boot.kernelPackages = pkgs.linuxPackagesFor pkgs.linux_latest;
      
      hardware.enableRedistributableFirmware = true;
      hardware.cpu.intel.updateMicrocode = true;

      hardware.opengl = {
       extraPackages = with pkgs; [
         amdvlk
       ];
       extraPackages32 = with pkgs; [
        driversi686Linux.amdvlk
       ];
      };

      ## Display settings
      os.output.primary.name = "DP-1";
      os.output.primary.width = 2560;
      os.output.primary.height = 1440;
      os.output.primary.hidpi = false;
      
      os.output.configs = [
        {
          name = "home";
          left = {
            name = "DP-3";
            id = "Philips Consumer Electronics Company PHL 245E1 0x0000630A";
            width = 2560;
            height = 1440;
          };
          right = {
            name = "DP-2";
            id = "Philips Consumer Electronics Company PHL 245E1 0x0000631B";
            width = 2560;
            height = 1440;
          };
        }
      ];
      
      ## Setting keymap to `de' for this machine.
      os.keyboard.layout = "us";

      ## Mail accounts in use on this machine
      mail.primary.enable = true;
      
      console = {
        font = "Lat2-Terminus16";
        keyMap = config.os.keyboard.layout;
      };

      users.users.${config.user} = {
        extraGroups = [ "wheel" "video" "input" ]; 
        isNormalUser = true;
      };
    })
    
    ## Host agnostic modules
    ##
    ## A list of file paths containing modules that should be used on this
    ## machine. They are not specific to this machine and can be used on
    ## other machines too as long as it fits their purpose.
    ../modules/common

    ## Hardware specific modules
    ../modules/hardware/backlight.nix

    ## Filesystems
    ../modules/common/system/filesystems/synology.nix

    ## Features
    ## Sets of modules for a specific purpose
    ../features/development.nix
    ../features/gaming.nix
    ../features/3d-printing.nix
    ../features/music.nix

    ## Chat
    ../modules/chat/discord.nix
    ../modules/chat/slack.nix
    ../modules/chat/signal.nix
  ];
}
