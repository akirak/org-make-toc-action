{
  description = "Run org-make-toc.el as an executable";

  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.pre-commit-hooks = {
    url = "github:cachix/pre-commit-hooks.nix";
    inputs.nixpkgs.follows = "nixpkgs";
    inputs.flake-utils.follows = "flake-utils";
  };
  inputs.gitignore = {
    url = "github:hercules-ci/gitignore.nix";
    inputs.nixpkgs.follows = "nixpkgs";
  };
  inputs.emacs-overlay = {
    url = "github:nix-community/emacs-overlay";
    inputs.nixpkgs.follows = "nixpkgs";
  };
  inputs.emacs-ci = {
    url = "github:akirak/nix-emacs-ci/add-flake";
    inputs.nixpkgs.follows = "nixpkgs";
    inputs.flake-utils.follows = "flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, gitignore, pre-commit-hooks, emacs-overlay, emacs-ci }:
    flake-utils.lib.eachSystem
      [
        "x86_64-linux"
        "x86_64-darwin"
        "aarch64-linux"
        "i686-linux"
      ]
      (system:
        let
          pkgs = import nixpkgs {
            inherit system;
            overlays = [
              emacs-overlay.overlay
            ];
          };
          inherit (gitignore.lib) gitignoreSource;
          src = gitignoreSource ./src;
          # Of emacs, emacsGit, and emacsGcc, emacs was the fastest
          # even in processing.
          emacs = (pkgs.emacsPackagesFor emacs-ci.packages.${system}.emacs-snapshot).emacsWithPackages
            (epkgs: [ epkgs.org-make-toc ]);
          # I thought portable dumping might make the startup process
          # faster, but it made little difference (0.38s -> 0.33s).
          #
          # dump = pkgs.runCommandNoCC "org-make-toc.pdump" {
          #   buildInputs = [
          #     emacs
          #   ];
          # } ''
          #   emacs -Q --batch -l org-make-toc --eval \
          #     "(dump-emacs-portable \"$out\")"
          # '';
          wrapper = pkgs.runCommandNoCC "org-make-toc"
            {
              inherit src;
              nativeBuildInputs = [
                pkgs.makeWrapper
              ];
              propagatedBuildInputs = [
                emacs
                src
              ];
            } ''
            mkdir -p $out/bin
            makeWrapper ${emacs}/bin/emacs $out/bin/org-make-toc \
              --add-flags "-Q --batch --script $src/runner.el"
          '';
        in
        rec {
          packages = flake-utils.lib.flattenTree {
            executable = wrapper;
            image = pkgs.dockerTools.buildImage {
              name = "org-make-toc";
              tag = "latest";
              created = "now";
              contents = wrapper;
              config.Cmd = [ "/bin/org-make-toc" ];
            };
          };
          defaultPackage = packages.executable;
          apps.org-make-toc = flake-utils.lib.mkApp {
            drv = packages.executable;
            name = "org-make-toc";
          };
          defaultApp = apps.org-make-toc;

          checks = {
            pre-commit-check = pre-commit-hooks.lib.${system}.run {
              src = gitignoreSource ./.;
              hooks = {
                nixpkgs-fmt.enable = true;
                nix-linter.enable = true;
              };
            };
          };
          devShell = nixpkgs.legacyPackages.${system}.mkShell {
            inherit (self.checks.${system}.pre-commit-check) shellHook;
          };
        }
      );
}
