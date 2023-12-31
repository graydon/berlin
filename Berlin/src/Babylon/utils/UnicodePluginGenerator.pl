#!/usr/bin/perl -w -IUnicodePluginGenerator
use Carp;
use Defined;
use Category;
use CombClass;
use Bidir;
use DecompClass;
use DecompString;
use DecDigitVal;
use DigitVal;
use NumericVal;
use Mirror;
use Upper;
use Lower;
use Title;
use Linebreak;
use EAWidth;
use Compositions;
use Block;
use Prop;

$UCD_File     = "UnicodeData.txt";
$Block_File   = "Blocks.txt";
$EA_File      = "EastAsianWidth.txt";
$LB_File      = "LineBreak.txt";
$Exclude_File = "CompositionExclusions.txt";
$Prop_File    = "PropList.txt";
$Prefix       = "./blocks/";

############################################################################

print "Reading data...\n";

print "  ...blocks\n";
# reading blocks...
open (BlockHandle, $Block_File) or die "Can't open Blocks!\n";
while(<BlockHandle>) {
  (my $info, my $rest) = split /#/;
  $info =~ s/([a-zA-Z0-9]*)\s*$/$1/;

  next if $info eq "";

  (my $chars, my $name) = split /; /, $info;
  (my $start, my $end) = split /\.\./, $chars;

  my $tmp = Block->new($start, $end, $name);

  push @blocks, $tmp;
}
close BlockHandle;

# reading data from the files...
print "  ...compositions\n";
my $COMP   = Compositions->new($UCD_File, $Exclude_File); print "  ...props\n";
my $PROPS  = Props->new($Prop_File); print "  ...categories\n";
my $CAT    = Category->new($UCD_File); print "  ...defines\n";
my $DEF    = Defined->new($UCD_File); print "  ...combining classes\n";
my $CCLASS = CombClass->new($UCD_File); print "  ...bidir properties\n";
my $BIDIR  = Bidir->new($UCD_File); print "  ...decomposition types\n";
my $DCLASS = DecompClass->new($UCD_File); print "  ...decomposition strings\n";
my $DSTR   = DecompString->new($UCD_File); print "  ...decimal digit values\n";
my $DDVAL  = DecDigitVal->new($UCD_File); print "  ...digit values\n";
my $DVAL   = DigitVal->new($UCD_File); print "  ...numeric values\n";
my $NVAL   = NumericVal->new($UCD_File); print "  ...mirroring properties\n";
my $MIRROR = Mirror->new($UCD_File); print "  ...uppercase equivalents\n";
my $UPPER  = Upper->new($UCD_File); print "  ...lowercase equivalents\n";
my $LOWER  = Lower->new($UCD_File); print "  ...titlecase equivalents\n";
my $TITLE  = Title->new($UCD_File); print "  ...linebreaking properties\n";
my $LB     = Linebreak->new($LB_File); print "  ...EA width properties\n";
my $EA     = EAWidth->new($EA_File);

print "Creating plugins...\n";

# ##########################################################################
# looping over the blocks
# ##########################################################################

foreach $block (@blocks) {
  printf "%s (%04X-%04X)\n", $block->name(), $block->start(), $block->end();

  # open output file
  my $output = ">".$Prefix.$block->filename();
  open (PLUGIN, $output) or
    die "Can't open $output for output!\n";

  $date = `date --rfc`; chop $date;

  # ########################################################################
  # print header...
  # ########################################################################
  printf PLUGIN
"/*\$Id: %s
 *
 * This source file is a part of the Berlin Project
 * Copyright (C) 1999 Tobias Hunger <tobias\@berlin-consortium.org>
 * http://www.berlin-consortium.org
 *
 * It was automatically created from the files available at
 * ftp.unicode.org on %s.
 *
 * This plugin to libPrague is free software; you can redistribute it
 * and/or  modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 675 Mass Ave, Cambridge,
 * MA 02139, USA
 */

#include <Babylon/defs.hh>
#include <Babylon/Dictionary.hh>
#include <bitset>
", $block->filename(), $date;

  print PLUGIN $DEF->include($block->start(), $block->end());
  print PLUGIN $UPPER->include($block->start(), $block->end());
  print PLUGIN $LOWER->include($block->start(), $block->end());
  print PLUGIN $TITLE->include($block->start(), $block->end());
  print PLUGIN $DDVAL->include($block->start(), $block->end());
  print PLUGIN $DVAL->include($block->start(), $block->end());
  print PLUGIN $NVAL->include($block->start(), $block->end());
  print PLUGIN $CAT->include($block->start(), $block->end());
  print PLUGIN $CCLASS->include($block->start(), $block->end());
  print PLUGIN $BIDIR->include($block->start(), $block->end());
  print PLUGIN $DCLASS->include($block->start(), $block->end());
  print PLUGIN $DSTR->include($block->start(), $block->end());
  print PLUGIN $MIRROR->include($block->start(), $block->end());
  print PLUGIN $LB->include($block->start(), $block->end());
  print PLUGIN $EA->include($block->start(), $block->end());
  print PLUGIN $COMP->include($block->start(), $block->end());
  print PLUGIN $PROPS->include($block->start(), $block->end());

  printf PLUGIN "
namespace Babylon {

  class %s : public Babylon::Dictionary::Block {
  public:
    void clean () {
    };
", $block->classname();

  printf PLUGIN "
    %s() {
      m_first_letter = %s;
      m_last_letter  = %s;
      // m_version=\"3.0.1\" // Not yet supported!
", $block->classname(), $block->start_string(), $block->end_string();

  print PLUGIN $DEF->init($block->start(), $block->end());
  print PLUGIN $UPPER->init($block->start(), $block->end());
  print PLUGIN $LOWER->init($block->start(), $block->end());
  print PLUGIN $TITLE->init($block->start(), $block->end());
  print PLUGIN $DDVAL->init($block->start(), $block->end());
  print PLUGIN $DVAL->init($block->start(), $block->end());
  print PLUGIN $NVAL->init($block->start(), $block->end());
  print PLUGIN $CAT->init($block->start(), $block->end());
  print PLUGIN $CCLASS->init($block->start(), $block->end());
  print PLUGIN $BIDIR->init($block->start(), $block->end());
  print PLUGIN $DCLASS->init($block->start(), $block->end());
  print PLUGIN $DSTR->init($block->start(), $block->end());
  print PLUGIN $MIRROR->init($block->start(), $block->end());
  print PLUGIN $LB->init($block->start(), $block->end());
  print PLUGIN $EA->init($block->start(), $block->end());
  print PLUGIN $COMP->init($block->start(), $block->end());
  print PLUGIN $PROPS->init($block->start(), $block->end());

  print PLUGIN "
    }

";

  printf PLUGIN "
    ~%s() {
    }
", $block->classname();

  # ########################################################################
  # print functions...
  # ########################################################################

  print PLUGIN "
    UCS4 firstLetter() {
      return m_first_letter;
    }

    UCS4 lastLetter() {
      return m_last_letter;
    }

    bool is_undef_block() const {
      return 0;
    }

    // query functions:
";

  printf PLUGIN "
    std::string blockname(const UCS4 uc) const {
      return \"%s\";
    }

", $block->name();

  print PLUGIN $DEF->function($block->start(), $block->end(),
			      $block->classname());
  print PLUGIN $UPPER->function($block->start(), $block->end(),
				$block->classname());
  print PLUGIN $LOWER->function($block->start(), $block->end(),
				$block->classname());
  print PLUGIN $TITLE->function($block->start(), $block->end(),
				$block->classname());
  print PLUGIN $DDVAL->function($block->start(), $block->end(),
				$block->classname());
  print PLUGIN $DVAL->function($block->start(), $block->end(),
			       $block->classname());
  print PLUGIN $NVAL->function($block->start(), $block->end(),
			       $block->classname());
  print PLUGIN $CAT->function($block->start(), $block->end(),
			      $block->classname());
  print PLUGIN $CCLASS->function($block->start(), $block->end(),
			      $block->classname());
  print PLUGIN $BIDIR->function($block->start(), $block->end(),
				$block->classname());
  print PLUGIN $DCLASS->function($block->start(), $block->end(),
				 $block->classname());
  print PLUGIN $DSTR->function($block->start(), $block->end(),
			       $block->classname());
  print PLUGIN $MIRROR->function($block->start(), $block->end(),
				 $block->classname());
  print PLUGIN $LB->function($block->start(), $block->end(),
			     $block->classname());
  print PLUGIN $EA->function($block->start(), $block->end(),
			     $block->classname());
  print PLUGIN $COMP->function($block->start(), $block->end(),
			       $block->classname());
  print PLUGIN $PROPS->function($block->start(), $block->end(),
				$block->classname());

  # ########################################################################
  # print variable defs...
  # ########################################################################

  printf PLUGIN "
  private:
    // functions
    %s(const %s &) {}

    Babylon\:\:UCS4 m_first_letter;
    Babylon\:\:UCS4 m_last_letter;
    // Babylon::UCS4_string m_version;
", $block->classname(), $block->classname();

  print PLUGIN $DEF->var_def($block->start(), $block->end());
  print PLUGIN $UPPER->var_def($block->start(), $block->end());
  print PLUGIN $LOWER->var_def($block->start(), $block->end()); 
  print PLUGIN $TITLE->var_def($block->start(), $block->end()); 
  print PLUGIN $CAT->var_def($block->start(), $block->end());
  print PLUGIN $CCLASS->var_def($block->start(), $block->end());
  print PLUGIN $BIDIR->var_def($block->start(), $block->end());
  print PLUGIN $DCLASS->var_def($block->start(), $block->end());
  print PLUGIN $DSTR->var_def($block->start(), $block->end());
  print PLUGIN $MIRROR->var_def($block->start(), $block->end());
  print PLUGIN $LB->var_def($block->start(), $block->end());
  print PLUGIN $EA->var_def($block->start(), $block->end());
  print PLUGIN $COMP->var_def($block->start(), $block->end());
  print PLUGIN $PROPS->var_def($block->start(), $block->end());

  # ########################################################################
  # print variables
  # ########################################################################

  printf PLUGIN "
  }; // class %s

", $block->classname(); 

  print PLUGIN $DEF->var($block->start(), $block->end(), $block->classname());
  print PLUGIN $UPPER->var($block->start(), $block->end(),
			   $block->classname());
  print PLUGIN $LOWER->var($block->start(), $block->end(),
			   $block->classname());
  print PLUGIN $TITLE->var($block->start(), $block->end(),
			   $block->classname());
  print PLUGIN $CAT->var($block->start(), $block->end(),
			 $block->classname());
  print PLUGIN $CCLASS->var($block->start(), $block->end(),
			    $block->classname());
  print PLUGIN $BIDIR->var($block->start(), $block->end(),
			   $block->classname());
  print PLUGIN $DCLASS->var($block->start(), $block->end(),
			    $block->classname());
  print PLUGIN $DSTR->var($block->start(), $block->end(),
			  $block->classname());
  print PLUGIN $MIRROR->var($block->start(), $block->end(),
			    $block->classname());
  print PLUGIN $LB->var($block->start(), $block->end(),
			$block->classname());
  print PLUGIN $EA->var($block->start(), $block->end(),
			$block->classname());
  print PLUGIN $COMP->var($block->start(), $block->end(),
			  $block->classname());
  print PLUGIN $PROPS->var($block->start(), $block->end(),
			   $block->classname());

  # ########################################################################
  # print footer...
  # ########################################################################
  printf PLUGIN "}; // namespace Babylon

dload(Babylon::%s);
", $block->classname();

  # close output file
  close PLUGIN;
} # foreach $block
