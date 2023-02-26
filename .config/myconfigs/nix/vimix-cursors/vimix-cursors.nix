{ pkgs ? import <nixpkgs> {} }:
with pkgs;

stdenv.mkDerivation rec {
  pname = "vimix-cursors";
  version = "2020-02-24";

  src = fetchFromGitHub {
    owner = "vinceliuice";
    repo = "Vimix-cursors";
    rev = version;
    # sha256 = "0000000000000000000000000000000000000000000000000000"; # 52 zeros
    sha256 = "TfcDer85+UOtDMJVZJQr81dDy4ekjYgEvH1RE1IHMi4=";
  };

  builder = ./builder.sh;
}
