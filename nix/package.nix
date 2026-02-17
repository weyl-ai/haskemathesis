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
      };

      packages.default = self'.packages.haskemathesis;
      checks.default = self'.packages.haskemathesis;
    };
}
