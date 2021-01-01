{ pkgs ? import ./nixpkgs.nix, compiler ? "ghc883", doBenchmark ? false, doCheck ? false }:
rec {
  docs = import ./docs/requirements.nix { inherit pkgs; };
  gi-gtk-declarative = (import ./gi-gtk-declarative { inherit compiler doBenchmark doCheck pkgs; }).gi-gtk-declarative;
  gi-gtk-declarative-app-simple = (import ./gi-gtk-declarative-app-simple { inherit compiler doBenchmark gi-gtk-declarative pkgs; }).gi-gtk-declarative-app-simple;
  gi-gtk-declarative-components = (import ./gi-gtk-declarative-components { inherit compiler doBenchmark gi-gtk-declarative pkgs; }).gi-gtk-declarative-components;
  examples = (import ./examples { inherit compiler doBenchmark gi-gtk-declarative gi-gtk-declarative-app-simple gi-gtk-declarative-components pkgs; }).examples;
}
