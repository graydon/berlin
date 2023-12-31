#! /usr/bin/perl

# Takes four arguments: include path, dependency file path, .idl path and
# .o path
#
# This script was ripped out of Warsaw's Makefile

$ipath = $ARGV[0];
$dpath = $ARGV[1];
$wpath = $ARGV[2];
$opath = $ARGV[3];

undef $/;
while (<STDIN>) {
    (/^(.*)\.idl\.o:((.|\n)*)/) || next;
    $file = $1;
    $dep = $2;
    print "$file", "SK.cc ", $file, "DynSK.cc ", "$ipath/Warsaw/", $file,
    ".hh $dpath/", $file, ".d:", $dep;
    $dep =~ s%$wpath%$ipath%g;
    $dep =~ s/\.idl/\.hh/g;
    print "$opath/", $file, "SK.o $opath/", $file, "DynSK.o", ": ", $dep;
}
