Each machine should have its own home configuration in
`home-<machine-name>.nix`. See existing files for examples of how to grab and
pass the configuration to `home-common.nix`.

In order to set a machine up, make a symbolic link from the
`~/.config/nixpkgs` directory to the `home-<machine-name>.nix`, naming the
resulting link `home.nix`.

After this, `home-manager switch` should build and enable the profile.
