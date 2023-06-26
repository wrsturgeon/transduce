{
  cargo,
  libiconv,
  naersk,
  pkg-config,
  rustc,
  rustfmt,
  targetPlatform,
}:
let
  cargo-toml = builtins.fromTOML (builtins.readFile ./Cargo.toml);
in
  naersk.lib."${targetPlatform.system}".buildPackage {
    CARGO_BUILD_INCREMENTAL = "false";
    RUST_BACKTRACE = "full";
    buildInputs = [ cargo libiconv pkg-config rustc rustfmt ];
    checkInputs = [ cargo rustc ];
    copyLibs = true;
    doCheck = true;
    meta = {
      description = cargo-toml.package.description;
      homepage = cargo-toml.package.homepage;
      license = cargo-toml.package.license;
      maintainers = cargo-toml.package.authors;
    };
    name = cargo-toml.package.name;
    src = ./.;
    version = cargo-toml.package.version;
  }
