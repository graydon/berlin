#!/usr/bin/perl

$DESTDIR = $ARGV[0];
$PREFIX = $ARGV[1];
$P = "$DESTDIR$PREFIX";

# *** Section 1 ***
# Install libraries
@libs = ( "Berlin", "Warsaw", "Prague" );
foreach $lib ( @libs ) {
  system ( "install -m644 lib/lib$lib.so $P/lib/" );
}

# Install kits
@kits = ( "Drawing", "Figure", "GLtt", "Layout", "Text", "Widget" );
foreach $kit ( @kits ) {
  system ( "install -m644 lib/lib$kit.so $P/lib/Berlin/Plugins/" );
}

# Install demos
@demos = ( "button", "draggable", "terminal" );
system( "install -m755 debian/demo $P/lib/Berlin/bin/demo" );
foreach $demo ( @demos ) {
  system( "install -m755 test/$demo $P/lib/Berlin/bin/$demo" );
  system( "cd $P/bin && ln -s ../lib/Berlin/bin/demo ./berlin-$demo" );
}

# Install server executable and wrapper script
system( "install -m755 test/server $P/lib/Berlin/bin/berlin.real" );
system( "install -m755 debian/serv-wrapper $P/bin/berlin" );

# *** End Section *********
# *** Section 2 ***********
# Install development files

# Install Warsaw IDL files
@IDL = `find include -name \'*.idl\'`;
chomp @IDL;
foreach $idl ( @IDL ) {
  system( "install -m644 $idl $P/include/Warsaw/IDL" );
}

# Install plugin headers
foreach $kit ( @kits ) {
  @HH = `find include/$kit -name \'*.h\'`;
  push @HH, `find include/$kit -name \'*.hh\'`;
  chomp @HH;
  foreach $header ( @HH ) {
    $header =~ /include\/(.*)/;
    my $suffix = $1;
    system( "install -m644 $header $P/include/Berlin/$suffix" );
  }
}

# Install library headers
foreach $lib ( @libs ) {
  @HH = `find include/$lib -name \'*.h\'`;
  push @HH, `find include/$lib -name \'*.hh\'`;
  chomp @HH;
  foreach $header ( @HH ) {
    system( "install -m644 $header $P/$header" );
  }
}

# *** End Section *****************
# *** Section 3 *******************
# Install documentation and sources
@html = `find doc/html -name '*.html'`;
chomp @html;
foreach $file ( @html ) {
  system( "install -m644 $file $P/doc/berlin-doc/" );
}

@sgml = `find doc -name '*.sgm'`;
chomp @sgml;
foreach $file ( @sgml ) {
  system( "install -m644 $file $P/doc/berlin-doc/docbook" );
}
