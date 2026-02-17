{ inputs, ... }:
{
  imports = [ inputs.haskell-flake.flakeModule ];

  perSystem =
    { self', pkgs, ... }:
    {
      haskellProjects.default = {
        settings = {
          haskemathesis.stan = true;
          librarySystemDepends = [ pkgs.zlib ];
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

      packages.default = self'.packages.haskemathesis;
      checks.default = self'.packages.haskemathesis;
    };
}
