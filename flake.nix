# Edited from https://hoverbear.org/blog/a-flake-for-your-crate/
let
  cargo-toml = builtins.fromTOML (builtins.readFile ./Cargo.toml);
in {
  description = cargo-toml.package.description;
  inputs = {
    naersk = {
      inputs.nixpkgs.follows = "nixpkgs";
      url = "github:NixOS/nmattia/naersk";
    };
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  };
  outputs = { naersk, nixpkgs, self }:
    let
      for-all-systems = f: nixpkgs.lib.genAttrs supported-systems (system: f system);
      supported-systems = [ "x86_64-linux" "aarch-linux" "x86_64-darwin" ];
    in {
      checks = for-all-systems (system:
        let
          pkgs = import nixpkgs { overlays = [ self.overlay ]; inherit system; };
        in {
          "${cargo-toml.package.name}" = pkgs."${cargo-toml.package.name}";
          format = let
            build-inputs = { buildInputs = with pkgs; [ rustfmt cargo ]; };
            commands = ''
              ${pkgs.rustfmt}/bin/cargo-fmt fmt --manifest-path ${./.}/Cargo.toml -- --check
              ${pkgs.nixpkgs-fmt} --check ${./.}
              touch $out
            '';
          in
            pkgs.runCommand "check-format" build-inputs commands;
        }
      );
      defaultPackage = for-all-systems (system: (import nixpkgs { overlays = [ self.overlay ]; inherit system; })."${cargo-toml.package.name}");
      devShell = for-all-systems (system:
        let
          pkgs = import nixpkgs {
            overlays = [ self.overlay ];
            inherit system;
          };
        in 
          pkgs.mkShell {
            inputsFrom = [ pkgs."${cargo-toml.package.name}" ];
            builtInputs = with pkgs; [ rustfmt nixpkgs-fmt ];
            LIBCLANG_PATH = "${pkgs.llvmPackages.libclang.lib}/lib";
          }
      );
      overlay = final: prev: { "${cargo-toml.package.name}" = final.callPackage ./. { inherit naersk; }; };
      packages = for-all-systems (system:
        let
          pkgs = import nixpkgs { overlays = [ self.overlay ]; inherit system; };
        in {
          "${cargo-toml.package.name}" = pkgs."${cargo-toml.package.name}";
        }
      );
    };
}
