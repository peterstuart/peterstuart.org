let
  cfg = import ../nix/default.nix { };
  hp = cfg.haskellPackages;
in
{}:

hp.shellFor {
  packages = p: [
    p.site
  ];

  buildInputs = with hp; [
    cabal-install
    ghcid
    hlint
    hp.site
    ormolu
  ];

  withHoogle = true;
}
