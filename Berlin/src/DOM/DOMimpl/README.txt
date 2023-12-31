This directory contains a C++ implementation of the
Document Object Model (DOM, http://www.w3c.org/).
It can be compiled and used both with and without
CORBA separately from Berlin.

The file Makefile, should be the same as Makefile_corba, which
is the official makefile, since Berlin uses CORBA.

If you want to compile DOM without CORBA, just copy
Makefile_nocorba to Makefile. Also remove the first
3 lines in the function main() in testIt.cc, which
is used to initialize CORBA.