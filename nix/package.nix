{ inputs, ... }:
{
  imports = [ inputs.haskell-flake.flakeModule ];

  perSystem =
    { self', ... }:
    {
      haskellProjects.default = {
        settings = {
          haskemathesis.stan = true;
        };
        devShell = {
          tools = hp: {
            cabal = hp.cabal-install;
          };
        };
      };

      packages.default = self'.packages.haskemathesis;
      checks.default = self'.packages.haskemathesis;
    };
}
