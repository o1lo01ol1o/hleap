(import ./default.nix).shellFor {
  tools = {
    cabal = "3.2.0.0";
    hlint = "3.2.7";
    haskell-language-server = "latest";
  };
}
