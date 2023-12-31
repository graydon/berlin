#!/usr/bin/perl -w

# this is so ugly that I don't want my name on it
# I guess that makes it public domain

# usage: gen-warsaw-idl.pl <list of idl files>
# output will be Warsaw.idl in the directory called from

use File::Basename;

$ignore = "out.idl";

$output = "Warsaw.idl";
#$header = "Warsaw.pl";

$deps = {};

open(OUT, "+>$output");
#open(HEADER, ">$header");

if (basename($ARGV[0]) ne $ARGV[0]) {
  print "Switching dirs to ", dirname($ARGV[0]), "\n";
  chdir(dirname($ARGV[0]));
}

map { $_ = basename($_) } @ARGV;
print "ARGV is ", join(", ", @ARGV), "\n";

foreach (@ARGV) {
  next if /$ignore/;
  $deps->{$_} = [];
}

push(@{$deps->{"ServerContext.idl"}}, "LifeCycle.idl");

foreach $file (keys %$deps) {
  open(IDL, "<$file");
  warn "Working on $file\n";
  while (<IDL>) {
#    if (/^interface (\S+);$/) {
#      push(@{$de->{$file}}, $1 . ".idl");
#    }
    if (/^interface .*: (.*)/) {
      $stuff = $1;
      $stuff =~ s/^(.*)\s+\{/$1/;
      $stuff =~ m/.+:\s+?(.+)\s+?\{?/;
      @stuff = split(/,\s*/, $stuff);
      map { $_ = $_ . ".idl" } @stuff;
#      warn "in $_, stuff is " . join(", ", @stuff) . "\n";
      push(@{$deps->{$file}}, @stuff);
#      push(@{$deps->{$file}}, $1 . ".idl") foreach /^interface .*:\s+(\S+)
    }
    if (/^#include [<"](\S+)[>"]$/) {
      push(@{$deps->{$file}}, $1);
    }
  }
  close(IDL);
}

sub member($$) {
    my ($item, $listref) = @_;

    foreach (@$listref) {
        if ($_ eq $item) {
            return 1;
        }
    }
    return 0;
}

@broke = ();

sub check_circles($@) {
  my ($currkey, @seen) = @_;
  foreach $dep (@{$deps->{$currkey}}) {
    if (member($dep, \@seen)) {
      push(@broke, $dep) if (!member($dep, \@broke));
    } else {
      check_circles($dep, (@seen, $dep));
    }
  }
} 
 
foreach $file (keys %$deps) {
  check_circles($file, ());
}
    
print "Circular: " . join(", ", @broke) . "\n";

#foreach $file (sort { $#{$deps->{$a}} <=> $#{$deps->{$b}} } (keys %$deps)) {
#  print "$file (" . join(", ", @{$deps->{$file}}) . "\n";
#}

%notdone = %$deps;

@order = ();

sub do_file($) {
  my $file = shift;

  if ($notdone{$file}) {
    $notdone{$file} = 0;
    foreach $dep (@{$deps->{$file}}) {
      do_file($dep);
    }
    push(@order, $file);
  }
}

foreach $file (keys %$deps) {
  do_file($file);
}

print "Order: " . join(", ", @order) . "\n";

foreach $file (@order) {
  open(IN, "<$file");
  while (<IN>) {
    if (! /^#include/) {
# MICO needs this; so will omniidl
      s/\b(fixed|supports)\b/_$1/;
      s#^(.*omniLifeCycleInfo.*)$#// $1#;
      s#^(.*Patch.*)$#// $1#;
      if (/module[^{]*$/) {   # they didn't put the { on the same line as the module declaration.  Hrmph.
        chomp;  # so we'll leave off the newline, and the next line will have it.  Or else bad things will happen.
        $_ .= " "; # and ensure that there's a space between the name and the bracket, so we can parse right
      }
      print OUT $_;
    }
  }
  close(IN);
}

exit;




print "Generated $output, making $header\n";

seek OUT, 0, 0 or die "Couldn't seek on OUT";

my @mods = ();
my @stack = ();

while (<OUT>) {
  next if (m#^//#);     # C++ style comments
  next if (m#^\s*\*#);  # these are C style comments
  if (/interface\s+(\S+)\s/) {
    $match = $1;
    $match =~ s/;$//;
    my $int = join("::", map { $_ if $_ } @stack);
    if ($int) {
      $int .= "::";
    }
    $int .= $match;
    if (!member($int, \@mods)) {
      print "Got interface $int\n";
      push(@mods, $int);
    } else {
      print "Duplicated interface $int\n";
    }
  }
  if (/{/ && /module\s+(\S+)\s/) {
    push(@stack, $1);
    next;  # ignore any { on this line
  }
  if (/{/) {
    foreach (/{/g) {
      push(@stack, "");
    }
  }
  if (/}/) {
    foreach (/}/g) {
      pop(@stack);
    }
  }
}
print "Modules: ", join(", ", @mods), "\n";

print HEADER "use lib 'Warsaw';\n\n";
print HEADER "use ", (split(/\./, basename($output)))[0], ";\n";
foreach (@mods) {
#  next if (m/CosNaming/);
  print HEADER "use $_;\n";
}
print HEADER "\n1;\n";


#print "Creating overall header\n";
#open(HEADER, ">$header");
#print HEADER "use lib 'Warsaw';\n\n";
#foreach $file (@order) {
#  next if $file =~ m/CosNaming/;
#  $file = basename($file);
#  $file =~ s/\.idl//;
#  print HEADER "use $file;\n";
#}
#print HEADER "\n1;\n";
#close(HEADER);

