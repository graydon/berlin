package NumericVal;
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

    if ($list[8] ne "") {
      $self->{hex($list[0])} = eval($list[8]);
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

  if    ($pos > 0x003400 and $pos < 0x004DB5) { $pos = 0x003400; }
  elsif ($pos > 0x004E00 and $pos < 0x009FA5) { $pos = 0x004E00; }
  elsif ($pos > 0x00AC00 and $pos < 0x00D7A3) { $pos = 0x00AC00; }
  elsif ($pos > 0x00D800 and $pos < 0x00DB7F) { $pos = 0x00D800; }
  elsif ($pos > 0x00DB80 and $pos < 0x00DBFF) { $pos = 0x00DB80; }
  elsif ($pos > 0x00DC00 and $pos < 0x00DFFF) { $pos = 0x00DC00; }
  elsif ($pos > 0x00E000 and $pos < 0x00F8FF) { $pos = 0x00E000; }
  elsif ($pos > 0x0F0000 and $pos < 0x0FFFFD) { $pos = 0x0F0000; }
  elsif ($pos > 0x100000 and $pos < 0x10FFFD) { $pos = 0x100000; }

  if(exists($self->{$pos})) {
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

  if($self->{_BL_START} != $bl_start or $self->{_BL_END} != $bl_end) {
    $self->{_BL_START} = $bl_start;
    $self->{_BL_END} = $bl_end;
    for (my $i = $bl_start; $i <= $bl_end; $i++) {
      if ($self->data($i) ne "undef") {
	$self->{_ATTENTION_NEEDED} = 1;
	last;
      }
      $self->{_ATTENTION_NEEDED} = 0;
    }
  }

  my $tmp = "    float numeric_value(const UCS4 uc) const {\n";

  if ($self->{_ATTENTION_NEEDED} == 1) {
    $tmp .= "      if (!is_defined(uc))\n";
    $tmp .= "        return 0;\n";
    $tmp .= "      switch(uc) {\n";
    for (my $i = $bl_start; $i <= $bl_end; $i++) {
      if($self->data($i) ne "undef") {
        $tmp .= sprintf "      case 0x%04Xu:\n        return %f;\n", 
	                $i, $self->{$i};
	$tmp .= "        break;\n";
      }
    }
    $tmp .= "      default:\n";
    $tmp .= "        return 0;\n";
    $tmp .= "      }\n";
  } else {
    $tmp .= "      return 0;\n";
  }
  $tmp   .= "    }\n\n";

  $tmp   .= "    bool is_Numeric(const UCS4 uc) const {\n";
   if ($self->{_ATTENTION_NEEDED} == 1) {
    $tmp .= "      switch(uc) {\n";
    for (my $i = $bl_start; $i <= $bl_end; $i++) {
      if($self->data($i) ne "undef") {
        $tmp .= sprintf "      case 0x%04Xu:\n",
	                $i;
      }
    }
    $tmp .= "        return 1;\n";
    $tmp .= "      default:\n";
    $tmp .= "        return 0;\n";
    $tmp .= "      }\n";
  } else {
    $tmp .= "      return 0;\n";
  }
  $tmp   .= "    }\n\n";

  return $tmp;
}

sub var_def {
  my $self = shift;
  return "";
}

sub var {
  my $self = shift;
  return "";
}

1; # for use to succeed...
