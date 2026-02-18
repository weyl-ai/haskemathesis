{ inputs, ... }:
{
  imports = [ inputs.haskell-flake.flakeModule ];

  perSystem =
    { self', pkgs, ... }:
    let
      pkg = self'.packages.haskemathesis.overrideAttrs (old: {
        meta = (old.meta or { }) // {
          mainProgram = "haskemathesis-cli";
        };
      });
    in
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
        default = pkg;
        docs = pkg.doc;
      };
      checks = {
        default = pkg;
        docs = pkg.doc;
      };
    };
}
