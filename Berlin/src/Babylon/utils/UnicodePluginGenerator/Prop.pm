package Props;
use strict;

sub new {
  my $self = {};
  my $prop_file = $_[1];
  my $currentProp = "";
  my %_props;
  my $skipThis = 1;
  my @propList;

  open(PROP, $prop_file) or die "Can't open Propertyfile (".$prop_file.")\n";
  while(<PROP>) {
    next if ($_ eq "\n"); # skip empty lines

    (my $info, my $rest) = split /#/;
    $info =~ s/([a-zA-Z0-9]*)\s*$/$1/; # remove trailing spaces

    next if ($info eq "");

    if ($info =~ /^([A-F0-9]+)\.\.([A-F0-9]+)\s*; ([\w\-]+)/) {
      if ($3 ne $currentProp) {
	$currentProp = $3;
	push @propList, $currentProp;
      }
      for (my $i = hex($1); $i <= hex($2); $i++) {
	$self->{$3}{$i} = 1;
      }
    } elsif ($info =~ /^([A-F0-9]+)(\s*); ([\w\-]+)/) {
      if ($3 ne $currentProp) {
	$currentProp = $3;
	push @propList, $currentProp;
      }
      $self->{$3}{hex($1)} = 1 if (!$skipThis);
    }
  }
  $self->{_PROP_LIST} = [ @propList ];

  close(PROP);

  bless($self);
  return $self;
}


sub data {
  my $self = shift;

  my $prop = $_[0];
  my $pos = $_[1];

  no strict 'refs';

  if(exists($self->{$prop}{$pos})) {
    return 1;
  }
  return 0;
}

sub include {
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

  my @propList = @{ $self->{_PROP_LIST} };
  my $tmp = "";

  foreach my $prop (@propList) {
    my $func = $prop;

    my $retval = $self->data($prop, $bl_start);
    my $attention = 0;
    for (my $i = $bl_start; $i <= $bl_end; $i++) {
      if ($self->data($prop, $i) != $retval) {
	$attention = 1;
      }
    }

    $tmp   .= "    bool is_".$func."(const UCS4 uc) const {\n";
    if ($attention) {
      $tmp .= "      return m_".$func.".test(uc - m_first_letter);\n";
    } else {
      $tmp .= "      return $retval;\n";
    }
    $tmp   .= "    }\n\n";
  }

  return $tmp;
}

sub var_def {
  my $self = shift;

  my $bl_start  = $_[0];
  my $bl_end    = $_[1];
  my $bl_length = $bl_end - $bl_start + 1;

  my @propList = @{ $self->{_PROP_LIST} };
  my $tmp = "";

  foreach my $prop (@propList) {
    my $func = $prop;

    my $retval = $self->data($prop, $bl_start);
    my $attention = 0;
    for (my $i = $bl_start; $i <= $bl_end; $i++) {
      if ($self->data($prop, $i) != $retval) {
	$attention = 1;
      }
    }

    if ($attention) {
      $tmp .= "    static const std::bitset<$bl_length> m_".$func.";\n";
    }
  }

  return $tmp;
}

sub var {
  my $self = shift;

  my $bl_start  = $_[0];
  my $bl_end    = $_[1];
  my $bl_name   = $_[2];
  my $bl_length = $bl_end - $bl_start + 1;

  my @propList = @{ $self->{_PROP_LIST} };
  my $tmp = "";

  foreach my $prop (@propList) {
    my $func = $prop;

    my $retval = $self->data($prop, $bl_start);
    my $attention = 0;
    for (my $i = $bl_start; $i <= $bl_end; $i++) {
      if ($self->data($prop, $i) != $retval) {
	$attention = 1;
      }
    }

    if ($attention) {
      $tmp .= "    const std::bitset<$bl_length> $bl_name\:\:m_$func(std::string(\"";
      my $str = "";
      for (my $i = $bl_start; $i <= $bl_end; $i++) {
	$str = $self->data($prop, $i).$str;
      }
      $tmp .= $str."\"));\n\n";
    }
  }

  return $tmp;
}

1; # for use to succeed...
