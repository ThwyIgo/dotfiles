{ stdenvNoCC, fetchFromGitHub, lib }:

stdenvNoCC.mkDerivation {
  pname = "dracula-theme-qt";
  version = "2022-03-21";

  src = fetchFromGitHub {
    owner = "dracula";
    repo = "qt5";
    rev = "7b25ee305365f6e62efb2c7aca3b4635622b778c";
    hash = "sha256-tfUjAb+edbJ+5qar4IxWr4h3Si6MIwnbCrwI2ZdUFAM=";
  };

  installPhase = ''
    runHook preInstall

    mkdir -p $out/share/qt5ct/colors/
    cp Dracula.conf $out/share/qt5ct/colors/

    runHook postInstall
  '';

  meta = with lib; {
    description = "Dracula Theme for qt5";
    homepage = "https://draculatheme.com/qt5";
    license = licenses.mit;
    platforms = platforms.all;
  };
}
