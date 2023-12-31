#! /bin/sh

source ../iconfig-funcs.sh

CONFIGURE_OPTIONS=""

query_word "Which ORB do you want to use? [omniORB] " --with-orb
query_word "Prefix for omniORB: [none] " --with-omniorb-prefix

echo $CONFIGURE_OPTIONS | sed 's/^ +//' > config
