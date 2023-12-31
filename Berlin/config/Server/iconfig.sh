#! /bin/sh

source ../iconfig-funcs.sh

CONFIGURE_OPTIONS=""

query_word "Build libBerlin? [yes] " --enable-berlin
query_line "Drawing Kits to build: [all] " --enable-drawing
query_line "Other Kits to build: [all] " --enable-kits
query_word "Build the server? [yes] " --enable-server
query_word "Prefix for FreeType 2: [none] " --with-freetype-prefix
query_word "Console to use: [GGI] " --with-console
query_word "Prefix for GGI: [none] " --with-ggi-prefix
query_word "Prefix for libArt: [none] " --with-art-prefix
query_word "Prefix for GGIMesa: [none] " --with-ggimesa-prefix

echo $CONFIGURE_OPTIONS | sed 's/^ +//' > config
