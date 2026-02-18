{ self, lib, ... }:
{
  perSystem =
    { pkgs, ... }:
    let
      isYaml = x: lib.hasPrefix "openapi" x && (lib.hasSuffix "yaml" x || lib.hasSuffix "yml" x);

      toPkg =
        openapiSpec:
        pkgs.runCommandLocal "openapi-docs"
          {
            nativeBuildInputs = [
              pkgs.redocly
            ];
          }
          ''
            redocly lint "${openapiSpec}" --lint-config error

            mkdir -p "$out/share/docs/openapi"

            redocly build-docs "${openapiSpec}" -o "$out/share/docs/openapi/index.html"
          '';

      openapiSpecs = lib.pipe self [
        lib.filesystem.listFilesRecursive
        (lib.filter isYaml)
        (map (file: {
          "openapi-spec-${builtins.hashString "sha256" file}" = toPkg file;
        }))
        lib.mergeAttrsList
      ];
    in
    {
      packages = openapiSpecs;
      checks = openapiSpecs;
    };
}
