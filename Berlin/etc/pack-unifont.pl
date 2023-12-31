#!/usr/bin/perl -w

#
# This just packs the unifont.hex file into a binary array which is
# then mmap()ed into the berlin process to provide a fallback unicode
# font.
#

open GLYPHS, ">glyph.dat";

while (<>) { $G{hex($1)}=$2 if /(....):(\S+)/; }
for ($code = 0; $code < 65536; $code++) {
  if (exists $G{$code}) 
    {
      my $str = $G{$code};
      
      $str2 = "";
      while ($str =~ /(..)/g) {
	$str2 = $1 . $str2;
      }
      $str = $str2;
      
      if (length($str) == 32) { 
	print GLYPHS pack (H2, "FF"); # halfwidth marker
	$str = $str2 . ("0" x 32); 
      } else {
	print GLYPHS pack (H2, "FE"); # fullwidth marker
	$str =~ s/(..)(..)/$2$1/go;
      }
      while ($str =~ /(..)/g) {
	print GLYPHS pack(H2, $1);
      }
    } else {
      print GLYPHS pack(H65, "0" x 65);
    }
}

