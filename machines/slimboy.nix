{
  inputs,
  ...
}:


inputs.nixpkgs.lib.nixosSystem {
  system = "x86_64-linux";
  modules = [
    inputs.home-manager.nixosModules.home-manager
    inputs.ordenada.nixosModules.ordenada
    (
      { pkgs, ... }:
      {
        imports = [
          ../modules/common
          ../modules/common/linux.nix
        ];

        networking.hostName = "slimboy";
        networking.interfaces.wlp0s20f3.useDHCP = false; # WiFi

        boot.kernelModules = [ "kvm-intel" ];
        boot.kernelPackages = pkgs.linuxPackagesFor pkgs.linux_latest;

        hardware.enableRedistributableFirmware = true;
        hardware.cpu.intel.updateMicrocode = true;
      }
    )
  ];
}
