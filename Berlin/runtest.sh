#!/bin/sh
#
# This just sets up the glyph database, sets some environment paths, 
# and runs a program from the test/ directory
#

export BERLIN_ROOT=`pwd`
export LD_LIBRARY_PATH=$BERLIN_ROOT/lib:$LD_LIBRARY_PATH

exec test/$1
