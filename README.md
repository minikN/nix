## NixOS Configuration
1. Add your machine specific setting under `./machines/yourmachine.nix` by copying another file and adjusting the settings (filename should be the `$HOSTNAME` of your machine).

2. Add your new machine in `./flake.nix` (towards the end).

3. Change your user name; full name and state version in `./flake.nix`

4. Enable/disable features in your new `yourmachine.nix`

5. Possible also disable modules in `modules/common/default.nix`

## Apply config
`cd /path/to/this/config` and
- First time: `HOSTNAME=yourmachine sudo nixos-rebuild switch --flake .#`
- Every other time: `sudo nixos-rebuild switch --flake .#`

## Update config
`cd /path/to/this/config` and `nix flake update`

## Bugs
- Mail not working correctly
- Output configuration / waybar for multi-monitor setup not working correctly
- Many more

## Credit
Most of my emacs configuration is taken from Andrew Tropin's [RDE](https://github.com/abcdw/rde/tree/master). Full credit goes to him and all the people contributing to the project. Check it out, it's a great project!