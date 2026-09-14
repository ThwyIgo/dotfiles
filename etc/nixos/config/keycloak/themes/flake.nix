{
  description = "Keycloak custom theme environment for Portal SSO";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };

        keycloak-theme-portal = pkgs.stdenv.mkDerivation {
          pname = "keycloak-theme-portal";
          version = "1.0.0";
          src = ./themes/portal;

          nativeBuildInputs = [ pkgs.zip ];

          buildPhase = ''
            runHook preBuild

            # Prepare staging structure for Keycloak Theme JAR (provider format)
            mkdir -p jar-staging/theme/portal
            cp -r * jar-staging/theme/portal/ 2>/dev/null || true
            rm -rf jar-staging/theme/portal/jar-staging

            mkdir -p jar-staging/META-INF
            cat << 'EOF_JSON' > jar-staging/META-INF/keycloak-themes.json
{
  "themes": [
    {
      "name": "portal",
      "types": ["login"]
    }
  ]
}
EOF_JSON

            (cd jar-staging && zip -r ../keycloak-theme-portal.jar META-INF theme)

            runHook postBuild
          '';

          installPhase = ''
            runHook preInstall

            # 1. NixOS services.keycloak.themes layout:
            # Theme types (e.g. login) directly at root of derivation output
            mkdir -p $out/login
            cp -r login/* $out/login/

            # 2. Keycloak Theme JAR (single file at root, ignored by NixOS dir iterator)
            cp keycloak-theme-portal.jar $out/keycloak-theme-portal.jar

            runHook postInstall
          '';

          meta = with pkgs.lib; {
            description = "Custom Keycloak theme for Portal Physis";
            platforms = platforms.all;
          };
        };
      in
      {
        packages = {
          default = keycloak-theme-portal;
          keycloak-theme-portal = keycloak-theme-portal;
        };
      });
}
