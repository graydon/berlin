#! /bin/sh

source ../iconfig-funcs.sh

CONFIGURE_OPTIONS=""

query_word "Prefix for omniORBpy: [none] " --with-omniorbpy-prefix

echo $CONFIGURE_OPTIONS | sed 's/^ +//' > config
