##
## `slimboy' configuration
##

{ inputs, globals, ... }:

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
    home-manager.nixosModules.home-manager
    
    ## System specific
    ##
    ## Inline function that returns the module continaing configuration
    ## specific to this machine. In order to make it a function we need
    ## to wrap it in ().
    ## TODO: Exclude all settings that are not system specific into
    ## their own modules.
    ({ lib, config, pkgs, ... }: {
      ## networking
      networking.hostName = "slimboy";
      networking.interfaces.enp0s20f0u1.useDHCP = true; # Ethernet dongle
      networking.interfaces.wlp0s20f3.useDHCP = true; # WiFi

      ## kernel
      boot.initrd.kernelModules = [ "vmd" ];
      boot.initrd.availableKernelModules = [ "xhci_pci" "thunderbolt" "vmd" "nvme" "usb_storage" "sd_mod" ];
      
      boot.kernelModules = [ "kvm-intel" ];
      boot.kernelPackages = pkgs.linuxPackagesFor pkgs.linux_latest;
      
      hardware.enableRedistributableFirmware = true;
      hardware.cpu.intel.updateMicrocode = true;

      # Set your time zone.
      time.timeZone = "Europe/Berlin";

      # Select internationalisation properties.
      i18n.defaultLocale = "en_US.UTF-8";
      console = {
        font = "Lat2-Terminus16";
        keyMap = "de";
      };

      users.users.${config.user} = {
        extraGroups = [ "wheel" "video" ]; 
        isNormalUser = true;
      };
    })
    
    ## Host agnostic modules
    ##
    ## A list of file paths containing modules that should be used
    ## on this machine. They are not specific to this machine and
    ## can be used on other machines too as long as it fits their
    ## purpose.
    ../modules/common
  ];
}
