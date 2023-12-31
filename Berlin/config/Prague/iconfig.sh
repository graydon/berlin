#! /bin/sh

source ../iconfig-funcs.sh

CONFIGURE_OPTIONS=""

query_word "Compile with tracer support? [no] " --enable-tracer

echo $CONFIGURE_OPTIONS | sed 's/^ +//' > config
