{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
    astal-haskell = {
      url = "github:aylur/astal/feat/haskell-bindings";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    haskell-gi = {
      url = "github:haskell-gi/haskell-gi";
      flake = false;
    };
  };

  outputs =
    inputs:
    inputs.flake-parts.lib.mkFlake { inherit inputs; } {
      imports = [ inputs.haskell-flake.flakeModule ];
      systems = inputs.nixpkgs.lib.systems.flakeExposed;
      perSystem =
        {
          pkgs,
          inputs',
          self',
          ...
        }:
        let
          pname = "mywidgets";
        in
        {
          packages.default = self'.packages.${pname};
          haskellProjects.default = {
            basePackages = pkgs.haskellPackages;
            otherOverlays = [
              (_: _: {
                #astal = inputs'.astal-haskell.packages.astal3;
                astal = pkgs.astal.astal3;
                astal-gtk4 = inputs'.astal-haskell.packages.astal4;
                astal-apps = inputs'.astal-haskell.packages.apps;
                astal-auth = inputs'.astal-haskell.packages.auth;
                astal-battery = inputs'.astal-haskell.packages.battery;
                astal-bluetooth = inputs'.astal-haskell.packages.bluetooth;
                astal-hyprland = inputs'.astal-haskell.packages.hyprland;
                astal-io = inputs'.astal-haskell.packages.io;
                astal-mpris = inputs'.astal-haskell.packages.mpris;
                astal-network = inputs'.astal-haskell.packages.network;
                astal-notifd = inputs'.astal-haskell.packages.notifd;
                astal-power-profiles = inputs'.astal-haskell.packages.powerprofiles;
                astal-river = inputs'.astal-haskell.packages.river;
                astal-tray = inputs'.astal-haskell.packages.tray;
                astal-wireplumber = inputs'.astal-haskell.packages.wireplumber;
              })
            ];
            devShell = {
              enable = true;
              hlsCheck.enable = true;
              tools = _: {
                just = pkgs.just;
              };
            };
            settings = {
              gi-nm = {
                broken = false;
              };
              # gi-astal = {
              #   buildFromSdist = false;
              #   check = false;
              #   stan = false;
              #   haddock = false;
              #   jailbreak = true;
              #   cabalFlags.with-generics = false;
              #   broken = false;
              # };
            };
            packages = {
              haskell-gi.source = inputs.haskell-gi;
              gi-astal.source = inputs.astal-haskell.outPath + "/lang/haskell/Astal";
              gi-astal-apps.source = inputs.astal-haskell.outPath + "/lang/haskell/AstalApps";
              gi-astal-auth.source = inputs.astal-haskell.outPath + "/lang/haskell/AstalAuth";
              gi-astal-battery.source = inputs.astal-haskell.outPath + "/lang/haskell/AstalBattery";
              gi-astal-bluetooth.source = inputs.astal-haskell.outPath + "/lang/haskell/AstalBluetooth";
              gi-astal-hyprland.source = inputs.astal-haskell.outPath + "/lang/haskell/AstalHyprland";
              gi-astal-io.source = inputs.astal-haskell.outPath + "/lang/haskell/AstalIO";
              gi-astal-mpris.source = inputs.astal-haskell.outPath + "/lang/haskell/AstalMpris";
              gi-astal-network.source = inputs.astal-haskell.outPath + "/lang/haskell/AstalNetwork";
              gi-astal-notifd.source = inputs.astal-haskell.outPath + "/lang/haskell/AstalNotifd";
              gi-astal-power-profiles.source = inputs.astal-haskell.outPath + "/lang/haskell/AstalPowerProfiles";
              gi-astal-river.source = inputs.astal-haskell.outPath + "/lang/haskell/AstalRiver";
              gi-astal-tray.source = inputs.astal-haskell.outPath + "/lang/haskell/AstalTray";
              gi-astal-wireplumber.source = inputs.astal-haskell.outPath + "/lang/haskell/AstalWp";
            };
          };
        };
    };
}
