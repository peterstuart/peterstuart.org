{ compiler ? "ghc884", pkgs }:
let
  inherit (pkgs.lib.trivial) flip pipe;
  inherit (pkgs.haskell.lib) appendPatch appendConfigureFlags dontCheck;

  hakyllFlags = [ "-f" "watchServer" "-f" "previewServer" ];

  haskellPackages = pkgs.haskell.packages.${compiler}.override {
    overrides = hpNew: hpOld: {
      hakyll = pipe hpOld.hakyll [
        (flip appendPatch ./hakyll.patch)
        (flip appendConfigureFlags hakyllFlags)
      ];

      pandoc-sidenote = hpNew.callCabal2nix "pandoc-sidenote"
        (builtins.fetchGit {
          url = "git@github.com:jez/pandoc-sidenote.git";
          rev = "766eb9db5c849b1007f5c310b8012e7da68e51a6";
        })
        { };

      site = hpNew.callCabal2nix "site" ./. { };

      # when hakyll is marked as broken in nixpkgs
      # because of version issues, fix them here:

      hslua = dontCheck (hpNew.callHackage "hslua" "1.0.3.2" { });
      jira-wiki-markup =
        dontCheck (hpNew.callHackage "jira-wiki-markup" "1.1.4" { });
      pandoc = dontCheck (hpNew.callHackage "pandoc" "2.9.2.1" { });
      pandoc-types = dontCheck (hpNew.callHackage "pandoc-types" "1.20" { });
    };
  };
in
haskellPackages
