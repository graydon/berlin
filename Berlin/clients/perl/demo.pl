#!/usr/bin/perl-thread -w

use strict;

use COPE::CORBA::ORB;
use Experimental::Exception;
use COPE::CORBA::Exception;

use Unicode::String qw(ucs2 utf8);
use Unicode::Map8;
use Thread qw(async);
use Data::Dumper;

use lib 'Warsaw';
##require 'include.pl';
#require 'Warsaw.pl';
use Warsaw::Warsaw;

use COPE::CosNaming;

use lib 'impl';
use ClientContext_impl;

BEGIN {
  use Carp;
  $SIG{__DIE__} = sub { confess(@_); };
}

$| = 1;

my $orb = CORBA::ORB_init($ARGV);
print "Got orb\n";

my $boa = $orb->BOA_init($ARGV);
# impl_is_ready doesn't return, so we have to spin it off in a thread
my $boa_thr = async {
  $boa->impl_is_ready(1);
};
$boa_thr->detach();
print "Got boa\n";

my $nsref = $ARGV[0] ? $ARGV[0] : "";
if (!$nsref) {
  open(NSREF, "<nsref.ior");
  $nsref = <NSREF>;
  close(NSREF);
}
my $context = CosNaming::NamingContext->_narrow($orb->string_to_object($nsref));
print "Got context\n";

#print "ServerContextManager interface: ", ServerContextManager->_interface(), "\n";
#my $obj = $context->resolve([ { id => ServerContextManager->_interface(), kind => "Object" } ]);
#my $manager = ServerContextManager->_narrow($obj);
#print "Got manager\n";
print "Server interface: ", Server->_interface(), "\n";
my $obj = $context->resolve([ { id => Server->_interface(), kind => "Object" } ]);
my $manager = Server->_narrow($obj);
print "Got server\n";

print "making ClientContext_impl\n";
my $client = new ClientContext_impl;
print "made, getting servercontext\n";
my $server = $manager->newServerContext($client);
print "Got servercontext, getting kits\n";
my $kits = { TextKit => '',
             DesktopKit => '',
             LayoutKit => '',
             ToolKit => '',
             WidgetKit => '',
             FigureKit => '',
             CommandKit => '',
             ImageKit => '', };
foreach my $kit (keys %$kits) {
  my $inter = "IDL:" . $kit . ":1.0";  # stupid hack, for now
  $kits->{$kit} = $server->resolve($inter, []);
  eval $kit . '->_narrow($kits->{' . $kit . '})';
  print "Got $kit\n";
}

my $vbox = $kits->{LayoutKit}->vbox();
print "Got vbox\n";

my $unimap = Unicode::Map8->new("US-ASCII");
my $string = ucs2($unimap->to16("Camels invade Germany, news at 11"));
my $label = $kits->{TextKit}->chunk([ $string->unpack() ]);
print "Got label\n";

use Cwd;
my $raster = $kits->{ImageKit}->create(getcwd() . "/berlin-perl.png");
print "Got raster\n";
my $pic = $kits->{FigureKit}->pixmap($raster);
print "Got pic\n";

$vbox->append($pic);
$vbox->append($kits->{LayoutKit}->hfil());
$vbox->append($label);

my $margin = $kits->{LayoutKit}->margin($vbox, 200);
print "Got margin\n";

my $bgcolor = new Color(red => .7, blue => .8, green => .6, alpha => 1);
my $outset = $kits->{ToolKit}->outset($margin, $bgcolor, 1);
print "Got outset\n";

my $group = $kits->{ToolKit}->group($margin);
print "Got group\n";
my $window = $kits->{DesktopKit}->shell($group);
print "Got window\n";
print "Done.  Ain't it purty?\n";
