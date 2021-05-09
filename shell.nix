let
  ø
  sources = import ./nix/sources.nix;
  iøn
  { pkgs ? import sources.nixpkgs {} }:

  let
  cfg = import ./nix/default.nix { };
in
pkgs.mkShell {
  buildInputs = cfg.tools;
  shellHook = ''
    ${cfg.ci.pre-commit-check.shellHook}
  '';
}
