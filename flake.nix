{
  description = "Rebar3 Diameter Compiler Plugin - Compile diameter .dia files in rebar3 projects";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        # Use beam packages for Erlang/OTP and rebar3
        beam = pkgs.beam.packagesWith pkgs.erlang;
        erlang = beam.erlang;
        rebar3 = beam.rebar3;

        # Build the rebar3 plugin
        rebar3_diameter_compiler = pkgs.stdenv.mkDerivation rec {
          pname = "rebar3_diameter_compiler";
          version = "0.8.1";

          src = ./.;

          nativeBuildInputs = [
            erlang
            rebar3
          ];

          buildPhase = ''
            # Prevent rebar3 from downloading dependencies
            export HOME=$TMPDIR

            # Build the project
            rebar3 compile
          '';

          installPhase = ''
            mkdir -p $out/lib/erlang/lib/${pname}-${version}

            # Copy compiled beam files
            cp -r _build/default/lib/${pname}/ebin $out/lib/erlang/lib/${pname}-${version}/

            # Copy source files if needed for reference
            cp -r src $out/lib/erlang/lib/${pname}-${version}/

            # Copy rebar configuration
            cp rebar.config $out/lib/erlang/lib/${pname}-${version}/
          '';

          meta = with pkgs.lib; {
            description = "Compile diameter .dia files in rebar3 projects";
            homepage = "https://github.com/carlosedp/rebar3_diameter_compiler";
            license = licenses.mit;
            maintainers = [ "Carlos Eduardo de Paula" ];
            platforms = platforms.unix;
          };
        };

      in
      {
        packages = {
          default = rebar3_diameter_compiler;
          rebar3_diameter_compiler = rebar3_diameter_compiler;
        };

        # Development shell with all necessary tools
        devShells.default = pkgs.mkShell {
          buildInputs = [
            erlang
            rebar3
            pkgs.git
          ];

          shellHook = ''
            echo "Rebar3 Diameter Compiler Plugin Development Environment"
            echo "Erlang/OTP version: $(erl -eval 'erlang:display(erlang:system_info(otp_release)), halt().' -noshell)"
            echo "Rebar3 version: $(rebar3 version)"
            echo ""
            echo "Available commands:"
            echo "  rebar3 compile - Compile the project"
            echo "  rebar3 eunit   - Run unit tests"
            echo "  rebar3 edoc    - Generate documentation"
            echo "  rebar3 clean   - Clean build artifacts"
          '';
        };

        # Apps for running common tasks
        apps = {
          compile = {
            type = "app";
            program = toString (
              pkgs.writeScript "compile" ''
                #!${pkgs.bash}/bin/bash
                cd ${./.}
                ${rebar3}/bin/rebar3 compile
              ''
            );
          };

          test = {
            type = "app";
            program = toString (
              pkgs.writeScript "test" ''
                #!${pkgs.bash}/bin/bash
                cd ${./.}
                ${rebar3}/bin/rebar3 eunit
              ''
            );
          };
        };
      }
    );
}
