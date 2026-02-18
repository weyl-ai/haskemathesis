{ inputs, ... }:
{
  imports = [ inputs.haskell-flake.flakeModule ];

  perSystem =
    { self', pkgs, ... }:
    {
      haskellProjects.default = {
        settings = {
          haskemathesis = {
            stan = true;
            haddock = true;
            librarySystemDepends = [ pkgs.zlib ];
          };
        };
        devShell = {
          tools = hp: {
            cabal = hp.cabal-install;
          };
          mkShellArgs = {
            packages = [ pkgs.zlib ];
          };
        };
      };

      packages = {
        default = self'.packages.haskemathesis;
        docs = self'.packages.haskemathesis.doc;
      };
      checks = {
        default = self'.packages.haskemathesis;
        docs = self'.packages.haskemathesis.doc;
      };
    };
}
