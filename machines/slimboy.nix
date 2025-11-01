{
  inputs,
  globals,
  ...
}:

inputs.nixpkgs.lib.nixosSystem {
  system = "x86_64-linux";
  specialArgs = { inherit inputs; };
  modules = [
    inputs.home-manager.nixosModules.home-manager
    inputs.ordenada.nixosModules.ordenada
    (
      { pkgs, lib, ... }:
      {
        imports = [
          (import ../modules/common {
            inherit
              inputs
              globals
              lib
              pkgs
              ;
          })
          ../modules/common/linux.nix
        ];

        networking.hostName = "slimboy";
        networking.interfaces.wlp4s0.useDHCP = true; # WiFi

        boot.kernelModules = [ "kvm-intel" ];
        boot.kernelPackages = pkgs.linuxPackagesFor pkgs.linux_latest;

        hardware.enableRedistributableFirmware = true;
        hardware.cpu.intel.updateMicrocode = true;

        ordenada.features = {
          #bluetooth.enable = true;
        };
      }
    )
  ];
}
