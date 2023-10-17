{ stdenv, fetchFromGitHub }:

stdenv.mkDerivation rec {
  pname = "vimix-cursors";
  version = "2020-02-24";

  src = fetchFromGitHub {
    owner = "vinceliuice";
    repo = "Vimix-cursors";
    rev = version;
    sha256 = "TfcDer85+UOtDMJVZJQr81dDy4ekjYgEvH1RE1IHMi4=";
  };

  installPhase = ''
    DEST_DIR="$out/usr/share/icons"

    mkdir -p $DEST_DIR
    cp -r dist/ $DEST_DIR/Vimix-cursors
    cp -r dist-white/ $DEST_DIR/Vimix-white-cursors
  '';
}
