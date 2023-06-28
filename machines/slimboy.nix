{ inputs, globals, ... }:

with inputs;

nixpkgs.lib.nixosSystem {
  system = "x86_64-linux";
  modules = [
    globals
    home-manager.nixosModules.home-manager
    {
      ## networking
      networking.hostName = "slimboy";
      networking.useDHCP = true;
      networking.networkmanager.enable = true;

      ## kernel
      boot.loader.systemd-boot.enable = true;
      boot.loader.efi.canTouchEfiVariables = true;
      
      boot.initrd.kernelModules = [ "vmd" ];
      boot.initrd.availableKernelModules = [ "xhci_pci" "thunderbolt" "vmd" "nvme" "usb_storage" "sd_mod" ];
      
      boot.kernelModules = [ "kvm-intel" ];
      boot.kernelPackages = pkgs.linuxPackagesFor pkgs.linux_latest;
      
      hardware.enableRedistributableFirmware = true;
      hardware.cpu.intel.updateMicrocode = true;

      ## File systems
      fileSystems."/" = {
        device = "/dev/disk/by-label/SYSTEM";
        fsType = "btrfs";
      };

      fileSystems."/boot" = {
        device = "/dev/disk/by-label/BOOT";
        fsType = "vfat";
      };

      swapDevices = [
        { device = "/dev/disk/by-label/SWAP"; }
      ];

      # Set your time zone.
      time.timeZone = "Europe/Berlin";

      # Select internationalisation properties.
      i18n.defaultLocale = "en_US.UTF-8";
      console = {
        font = "Lat2-Terminus16";
        keyMap = "de";
      };

      users.users.${config.user} = {
        extraGroups = [ "wheel" "networking" "video" ]; 
      };
      users.users.${config.user}.isNormalUser = true;
    }
  ];
}
