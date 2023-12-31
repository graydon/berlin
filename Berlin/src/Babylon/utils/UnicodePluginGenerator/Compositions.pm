package Compositions;
use strict;

sub new {
  my $self = {};

  my $ucd_file = $_[1];
  my $excl_file= $_[2];

  my @excl;

  open(EXCL, $excl_file) or die "Can't open ".$excl_file."\n";

  while(<EXCL>) {
    my $line = chop;
    if ($line eq "") { next; }
    (my $info, my $rest) = split /#/, 2;
    if ($info eq "") { next; }

    $info =~ s/([a-zA-Z0-9]*)\s*$/$1/; # remove trailing spaces

    next if ($info eq "");

    push @excl, $info;
  }

  close(EXCL);

  open(UCD, $ucd_file);

  while(<UCD>) {
    my $line = chop;
    (my $info, my $rest) = split /#/;
    $info =~ s/([a-zA-Z0-9]*)\s*$/$1/; # remove trailing spaces

    next if ($info eq "");

    my @list = split /;/, $info, 15;

    my $decomp = $list[5];
    my $dType = "";
    my $tmp = "";

    if ($decomp =~ /^<(\w+)>$/) {
      $tmp = "";
    } elsif ($decomp =~ /^<(\w+)> ([A-F0-9 ]*)$/) {
      $tmp = "";
    } else {
      $tmp = $decomp;
    }

    if (($tmp =~ tr/ / /) == 1) {
      if (!(grep {$_ eq $list[0]} @excl)) {
	my @chars = split / /, $tmp;
	$tmp = sprintf "%08X%08X", hex($chars[0]), hex($chars[1]);
	$self->{$tmp} = $list[0];
      }
    }
  }

  close(UCD);

  $self->{_BL_START} = -1;
  $self->{_BL_END} = -1;
  $self->{_ATTENTION_NEEDED} = 1;

  bless($self);
  return $self;
}


sub data {
  my $self = shift;

  my $pos = $_[0];
  my $result = "";

  if(exists($self->{$pos})) {
    return $self->{$pos};
  } else {
    return "undef";
  }
}

sub include {
  my $self = shift;

  my $bl_start = $_[0];
  my $bl_end   = $_[1];

  if((sprintf "%04X", $bl_start) eq "AC00") { # hangul
    return "";
  }

  my @blockkeys = sort grep {$_ ne "_ATTENTION_NEEDED" and
			     $_ ne "_BL_START" and
			     $_ ne "_BL_END" and
			     $_ ne "_INPUT_MAX_LENGTH" and
			     $_ ne "_RES_MAX_LENGTH" and
                             hex(substr $_, 0, 8) >= $bl_start and
		             hex(substr $_, 0, 8) <= $bl_end}
                  keys %$self;
  if($self->{_BL_START} != $bl_start or $self->{_BL_END} != $bl_end) {
    $self->{_BL_START} = $bl_start;
    $self->{_BL_END} = $bl_end;

    if ($#blockkeys > 0) {
      $self->{_ATTENTION_NEEDED} = 1;
    } else {
      $self->{_ATTENTION_NEEDED} = 0;
    }
  }

  if ($self->{_ATTENTION_NEEDED} == 1) {
    return "#include <map>\n";
  }

  return "";
}

sub init {
  my $self = shift;
  my $bl_start = $_[0];
  my $bl_end   = $_[1];

  if((sprintf "%04X", $bl_start) eq "AC00") { # hangul
    return "";
  }

  my @blockkeys = sort grep {$_ ne "_ATTENTION_NEEDED" and
			     $_ ne "_BL_START" and
			     $_ ne "_BL_END" and
			     $_ ne "_INPUT_MAX_LENGTH" and
			     $_ ne "_RES_MAX_LENGTH" and
                             hex(substr $_, 0, 8) >= $bl_start and
		             hex(substr $_, 0, 8) <= $bl_end}
                  keys %$self;
  if($self->{_BL_START} != $bl_start or $self->{_BL_END} != $bl_end) {
    $self->{_BL_START} = $bl_start;
    $self->{_BL_END} = $bl_end;

    if ($#blockkeys > 0) {
      $self->{_ATTENTION_NEEDED} = 1;
    } else {
      $self->{_ATTENTION_NEEDED} = 0;
    }
  }

  if ($self->{_ATTENTION_NEEDED} == 1) {
    my $tmp = "";
    foreach my $i (@blockkeys) {
      $tmp .= sprintf "      m_composeMap\[make_pair(0x%s, 0x%s)\] = 0x%s;\n",
      substr($i, 0, 8), substr($i, 8, 8),  $self->data($i);
    }
    return $tmp;
  }
  return "";
}

sub function {
  my $self = shift;
  my $bl_start = $_[0];
  my $bl_end   = $_[1];

  if((sprintf "%04X", $bl_start) eq "AC00") { # hangul
    my $tmp = "    UCS4 compose (const UCS4 starter, const UCS4 last) {\n";
    $tmp .= "      const UCS4 sBase = 0xAC00;\n";
    $tmp .= "      const UCS4 lBase = 0x1100;\n";
    $tmp .= "      const UCS4 vBase = 0x1161;\n";
    $tmp .= "      const UCS4 tBase = 0x11A7;\n\n";
    $tmp .= "      const int lCount = 19;\n";
    $tmp .= "      const int vCount = 21;\n";
    $tmp .= "      const int tCount = 28;\n";
    $tmp .= "      const int nCount = vCount * tCount;\n";
    $tmp .= "      const int sCount = lCount * nCount;\n\n";

    $tmp .= "      UCS4 result = 0;\n";
    $tmp .= "      // check if the characters are L and V\n";
    $tmp .= "      int lIndex = starter - vBase;\n";
    $tmp .= "      if ( 0 <= lIndex && lIndex < lCount ) {\n";
    $tmp .= "        int vIndex = last - vBase;\n";
    $tmp .= "        if ( 0 <= vIndex && vIndex < vCount ) {\n";
    $tmp .= "          // make syllable form LV\n";
    $tmp .= "          result = UCS4(sBase + (lIndex * vCount + vIndex) * tCount);\n";
    $tmp .= "        }\n";
    $tmp .= "      }\n\n";
    $tmp .= "      // check if the characters are LV and T\n";
    $tmp .= "      int sIndex = starter - sBase;\n";
    $tmp .= "      if (0 <= sIndex && sIndex < sCount && (sIndex % tCount) == 0) {\n";
    $tmp .= "        int tIndex = last - tBase;\n";
    $tmp .= "        if (0 <= tIndex && tIndex <= tCount)\n";
    $tmp .= "          result = starter + tIndex;\n";
    $tmp .= "      }\n\n";
    $tmp .= "      return result;\n";
    $tmp .= "    }\n\n";

    return $tmp;
  }

  my @blockkeys = sort grep {$_ ne "_ATTENTION_NEEDED" and
			     $_ ne "_BL_START" and
			     $_ ne "_BL_END" and
			     $_ ne "_INPUT_MAX_LENGTH" and
			     $_ ne "_RES_MAX_LENGTH" and
                             hex(substr $_, 0, 8) >= $bl_start and
		             hex(substr $_, 0, 8) <= $bl_end}
                  keys %$self;

  if($self->{_BL_START} != $bl_start or $self->{_BL_END} != $bl_end) {
    $self->{_BL_START} = $bl_start;
    $self->{_BL_END} = $bl_end;

    if ($#blockkeys > 0) {
      $self->{_ATTENTION_NEEDED} = 1;
    } else {
      $self->{_ATTENTION_NEEDED} = 0;
    }
  }

  my $tmp = "    UCS4 compose (const UCS4 start, const UCS4 last) {\n";

  if ($self->{_ATTENTION_NEEDED} == 1) {
    $tmp .= "      return m_composeMap[make_pair(start, last)];\n";
  } else {
    $tmp .= "      return 0;\n";
  }
  $tmp   .= "    }\n\n";
  return $tmp;
}

sub var_def {
  my $self = shift;

  my $bl_start = $_[0];
  my $bl_end   = $_[1];

  if((sprintf "%04X", $bl_start) eq "AC00") { # hangul
    return "";
  }

  my @blockkeys = sort grep {$_ ne "_ATTENTION_NEEDED" and
			     $_ ne "_BL_START" and
			     $_ ne "_BL_END" and
			     $_ ne "_INPUT_MAX_LENGTH" and
			     $_ ne "_RES_MAX_LENGTH" and
                             hex(substr $_, 0, 8) >= $bl_start and
		             hex(substr $_, 0, 8) <= $bl_end}
                  keys %$self;
  if($self->{_BL_START} != $bl_start or $self->{_BL_END} != $bl_end) {
    $self->{_BL_START} = $bl_start;
    $self->{_BL_END} = $bl_end;

    if ($#blockkeys > 0) {
      $self->{_ATTENTION_NEEDED} = 1;
    } else {
      $self->{_ATTENTION_NEEDED} = 0;
    }
  }

  if ($self->{_ATTENTION_NEEDED} == 1) {
    my $inlength = $self->{_INPUT_MAX_LENGTH};
    my $outlength = $self->{_RES_MAX_LENGTH};
    my $tmp = "    std::map<pair<UCS4, UCS4>, UCS4> m_composeMap;\n";
    return $tmp;
  }

  return "";
}

sub var {
  return "";
}

1; # for use to succeed...
