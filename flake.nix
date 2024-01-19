{
  description = "logjuicer-website";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
    logjuicer.url = "github:logjuicer/logjuicer";
  };

  outputs = inputs:
    inputs.flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import inputs.nixpkgs { inherit system; };

        haskellExtend = hpFinal: hpPrev: {
          logjuicer-github-io =
            hpPrev.callCabal2nix "logjuicer-github-io" inputs.self { };
        };
        hsPkgs = pkgs.haskellPackages.extend haskellExtend;

        genSources = pkgs.writeScriptBin "gen-sources" ''
          mkdir -p generated
          cat ${inputs.logjuicer}/README.md > generated/doc.md
          ${inputs.logjuicer.defaultPackage.x86_64-linux}/bin/logjuicer --help > generated/cli-help.txt
          ${inputs.logjuicer.defaultPackage.x86_64-linux}/bin/logjuicer --version > generated/cli-version.txt
        '';

        website = pkgs.stdenv.mkDerivation {
          name = "changemetrics.io-pages";
          buildInputs = [ hsPkgs.logjuicer-github-io ];
          src = inputs.self;
          # https://github.com/jaspervdj/hakyll/issues/614
          # https://github.com/NixOS/nix/issues/318#issuecomment-52986702
          # https://github.com/MaxDaten/brutal-recipes/blob/source/default.nix#L24
          LOCALE_ARCHIVE =
            pkgs.lib.optionalString (pkgs.buildPlatform.libc == "glibc")
            "${pkgs.glibcLocales}/lib/locale/locale-archive";
          LANG = "en_US.UTF-8";

          buildPhase = ''
            logjuicer-github-io build
          '';
          installPhase = ''
            mv _site $out
          '';
        };
      in {
        packages.website = website;
        devShell = hsPkgs.shellFor {
          packages = p: [ p.logjuicer-github-io ];
          buildInputs = [ pkgs.cabal-install genSources pkgs.pandoc ];
        };
      });
}
