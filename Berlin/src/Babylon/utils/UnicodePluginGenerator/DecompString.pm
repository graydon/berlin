package DecompString;
use strict;

sub new {
  my $self = {};
  my $ucd_file = $_[1];

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
      $tmp = $2;
    } else {
      $tmp = $decomp;
    }

    if ($tmp ne "") {
      $self->{hex($list[0])} = $tmp;
    }
  }

  close(UCD);

  $self->{_BL_START} = -1;
  $self->{_BL_END} = -1;
  $self->{_ATTENTION_NEEDED} = 1;
  $self->{_MAX_LENGTH} = 2;

  bless($self);
  return $self;
}


sub data {
  my $self = shift;

  my $pos = $_[0];
  my $result = "";

  if    ($pos > 0x003400 and $pos < 0x004DB5) {
    return "undef"; }
  elsif ($pos > 0x004E00 and $pos < 0x009FA5) {
    return "undef"; }
  elsif ($pos > 0x00AC00 and $pos < 0x00D7A3) {
    return "undef"; }
  elsif ($pos > 0x00D800 and $pos < 0x00DB7F) {
    return "undef"; }
  elsif ($pos > 0x00DB80 and $pos < 0x00DBFF) {
    return "undef"; }
  elsif ($pos > 0x00DC00 and $pos < 0x00DFFF) {
    return "undef"; }
  elsif ($pos > 0x00E000 and $pos < 0x00F8FF) {
    return "undef"; }
  elsif ($pos > 0x0F0000 and $pos < 0x0FFFFD) {
    return "undef"; }
  elsif ($pos > 0x100000 and $pos < 0x10FFFD) {
    return "undef"; }
  elsif(exists($self->{$pos})) {
    return $self->{$pos};
  } else {
    return "undef";
  }
}

sub include{
  return "";
}

sub init {
  return "";
}

sub function {
  my $self = shift;

  my $bl_start = $_[0];
  my $bl_end   = $_[1];
  my $bl_name  = $_[2];

  my $tmp = "    UTF32_string decompose(const UCS4 uc) const {\n";

  if((sprintf "%04X", $bl_start) eq "AC00") { # hangul
    $tmp .= "      const UCS4 sBase = 0xAC00;\n";
    $tmp .= "      const UCS4 lBase = 0x1100;\n";
    $tmp .= "      const UCS4 vBase = 0x1161;\n";
    $tmp .= "      const UCS4 tBase = 0x11A7;\n\n";
    $tmp .= "      const int lCount = 19;\n";
    $tmp .= "      const int vCount = 21;\n";
    $tmp .= "      const int tCount = 28;\n";
    $tmp .= "      const int nCount = vCount * tCount;\n";
    $tmp .= "      const int sCount = lCount * nCount;\n\n";
    $tmp .= "      int sIndex = uc - sBase;\n\n";
    $tmp .= "      if (sIndex < 0 || sIndex >= sCount) {\n";
    $tmp .= "        UTF32_string us; us.resize(1); us[0]=uc;\n";
    $tmp .= "        return us;\n\n";
    $tmp .= "      }\n";
    $tmp .= "      UTF32_string res;\n";
    $tmp .= "      UCS4 l = lBase + sIndex / nCount;\n";
    $tmp .= "      UCS4 v = vBase + (sIndex % nCount) / tCount;\n";
    $tmp .= "      UCS4 t = tBase + sIndex % tCount;\n\n";
    $tmp .= "      res += l; res += v;\n";
    $tmp .= "      if (t != tBase) res += t;\n\n";
    $tmp .= "      return res;\n";
    $tmp .= "    }\n\n";
    return $tmp;
  }
  if($self->{_BL_START} != $bl_start or $self->{_BL_END} != $bl_end) {
    $self->{_BL_START} = $bl_start;
    $self->{_BL_END} = $bl_end;
    $self->{_ELEM} = "";
    $self->{_ATTENTION_NEEDED} = 0;
    $self->{_MAX_LENGTH} = 2;

    for (my $i = $bl_start; $i <= $bl_end; $i++) {
      if ($self->data($i) ne "undef") {
	my $tmpstr = $self->data($i);
	if (($tmpstr =~ tr/ / /) > $self->{_ATTENTION_NEEDED} - 1) {
	  $self->{_ATTENTION_NEEDED} = ($tmpstr =~ tr/ / /) + 1;
	}
	if (hex($bl_end) > 0xFFFF) {
	  $self->{_MAX_LENGTH} = 4;
	} else {
	  my @datas = split / /, $self->data($i);
	  foreach my $data (@datas) {
	    if (hex($data) > 0xFFFF) {
	      $self->{_MAX_LENGTH} = 4;
	    }
	  }
	}
      }
    }
  }

  if ($self->{_ATTENTION_NEEDED} == 1) {
    $tmp .= "      Babylon::UTF32_string us;\n";
    $tmp .= "      us.resize(1);\n";
    $tmp .= "      us\[0\] = $bl_name\:\:m_decompStr\[uc - m_first_letter\];\n";
  }
  elsif ($self->{_ATTENTION_NEEDED} >= 2) {
    $tmp .= "      Babylon::UTF32_string us;\n";
    $tmp .= "      us.resize(2);\n";
    $tmp .= "      us\[0\] = $bl_name\:\:m_decompStr\[uc - m_first_letter\]\[0\];\n";
    $tmp .= "      us\[1\] = $bl_name\:\:m_decompStr\[uc - m_first_letter\]\[1\];\n";
    if ($self->{_ATTENTION_NEEDED} >= 3) {
      $tmp .= "\n      switch (uc) {\n";

      for (my $i = $bl_start; $i <= $bl_end; $i++) {
	my @tmpstr = split / /, $self->data($i);
	my $data = $self->data($i);
	if(($data =~ tr/ / /) > 1) {
	  $tmp .= sprintf "\n      case 0x%04X:\n        us.resize(%d);\n",
	                  $i, ($data =~ tr/ / /) + 1;
 	  for (my $j = 2; $j <= ($data =~ tr/ / /); $j++) {
	    $tmp .= sprintf "        us\[%du\] = 0x%su;\n", $j, $tmpstr[$j];
	  }
             $tmp.= "        break;\n";
        }
      }
      $tmp .= "      }\n";
    }
    $tmp .= "      if (us[1] == 0x0000u) {\n";
    $tmp .= "        us.resize(1);\n";
    $tmp .= "      }\n\n";
  } else {
    $tmp .= "      UTF32_string us;\n";
    $tmp .= "      us.resize(1); us[0] = uc;\n";
  }
  $tmp .=   "      return us;\n";
  $tmp .=   "    }\n\n";
  return $tmp;
}

sub var_def {
  my $self = shift;

  my $bl_start = $_[0];
  my $bl_end   = $_[1];
  my $bl_length = $bl_end - $bl_start + 1;

  if($self->{_BL_START} != $bl_start or $self->{_BL_END} != $bl_end) {
    $self->{_BL_START} = $bl_start;
    $self->{_BL_END} = $bl_end;
    $self->{_ELEM} = "";
    $self->{_ATTENTION_NEEDED} = 0;
    $self->{_MAX_LENGTH} = 2;

    for (my $i = $bl_start; $i <= $bl_end; $i++) {
      if ($self->data($i) ne "undef") {
	my $tmpstr = $self->data($i);
	if (($tmpstr =~ tr/ / /) > $self->{_ATTENTION_NEEDED} - 1) {
	  $self->{_ATTENTION_NEEDED} = ($tmpstr =~ tr/ / /) + 1;
	}
	if (hex($bl_end) > 0xFFFF) {
	  $self->{_MAX_LENGTH} = 4;
	} else {
	  my @datas = split / /, $self->data($i);
	  foreach my $data (@datas) {
	    if (hex($data) > 0xFFFF) {
	      $self->{_MAX_LENGTH} = 4;
	    }
	  }
	}
      }
    }
  }

  my $len = $self->{_MAX_LENGTH};
  if ($self->{_ATTENTION_NEEDED} == 1) {
    return "    static const UCS$len m_decompStr\[$bl_length\];\n";
  } elsif ($self->{_ATTENTION_NEEDED} > 1) {
    return "    static const UCS$len m_decompStr\[$bl_length\][2];\n";
  } else {
    return "";
  }
}

sub var {
  my $self = shift;

  my $bl_start = $_[0];
  my $bl_end   = $_[1];
  my $bl_name  = $_[2];

  if($self->{_BL_START} != $bl_start or $self->{_BL_END} != $bl_end) {
    $self->{_BL_START} = $bl_start;
    $self->{_BL_END} = $bl_end;
    $self->{_ELEM} = "";
    $self->{_ATTENTION_NEEDED} = 0;
    $self->{_MAX_LENGTH} = 2;

    for (my $i = $bl_start; $i <= $bl_end; $i++) {
      if ($self->data($i) ne "undef") {
	my $tmpstr = $self->data($i);
	if (($tmpstr =~ tr/ / /) > $self->{_ATTENTION_NEEDED} - 1) {
	  $self->{_ATTENTION_NEEDED} = ($tmpstr =~ tr/ / /) + 1;
	}
	if (hex($bl_end) > 0xFFFF) {
	  $self->{_MAX_LENGTH} = 4;
	} else {
	  my @datas = split / /, $self->data($i);
	  foreach my $data (@datas) {
	    if (hex($data) > 0xFFFF) {
	      $self->{_MAX_LENGTH} = 4;
	    }
	  }
	}
      }
    }
  }

  my $len = $self->{_MAX_LENGTH};
  if ($self->{_ATTENTION_NEEDED} == 1) {
    my $tmp = "  const UCS$len $bl_name\:\:m_decompStr\[\] = {";
    for (my $i= $bl_start; $i <= $bl_end; $i++) {
      my $data = $self->data($i);
      if (($i - $bl_start) % 4 == 0) {
	$tmp .= "\n    ";
      }
      if ($data eq "undef") {
	$tmp .= sprintf "0x%04Xu", $i;
      } elsif (($data =~ tr/ / /) == 0) {
	$tmp .= "0x".$data."u";
      }
      if ( $i != $bl_end) {
	$tmp .= ", ";
      }
    }
    $tmp .= "\n  };\n\n";
    return $tmp;
  } elsif ($self->{_ATTENTION_NEEDED} > 1) {
    my $tmp = "  const UCS$len $bl_name\:\:m_decompStr\[\]\[2\] = {";
    for (my $i= $bl_start; $i <= $bl_end; $i++) {
      my $data = $self->data($i);
      if (($i - $bl_start) % 4 == 0) {
	$tmp .= "\n    ";
      }
      if ($data eq "undef") {
	$tmp .= sprintf "{ 0x%04Xu, 0x0000u }", $i;
      } elsif (($data =~ tr/ / /) == 0) {
	$tmp .= "{ 0x".$data."u, 0x0000u }";
      } elsif (($data =~ tr/ / /) >= 1) {
	my @tmpstr = split / /, $data;
	$tmp .= "{ 0x".$tmpstr[0]."u, 0x".$tmpstr[1]."u }";
      }
      if ( $i != $bl_end) {
	$tmp .= ", ";
      }
    }
    $tmp .= "\n  };\n\n";
    return $tmp;
  } else {
    return "";
  }
}

1; # for use to succeed...
