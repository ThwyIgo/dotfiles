source $stdenv/setup

DEST_DIR="$out/usr/share/icons"

installPhase() {
    mkdir -p $DEST_DIR
    cp -r dist/ $DEST_DIR/Vimix-cursors
    cp -r dist-white/ $DEST_DIR/Vimix-white-cursors
}

genericBuild
